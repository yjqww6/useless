#lang racket
(require syntax/parse (for-syntax syntax/parse))

(module text racket
  (provide (all-defined-out))

  (define (just str orig?)
    (if orig?
        str
        (string-append "(" str ")")))
  
  (define (un-improper str orig?)
    (if orig?
        (string-append ". " str)
        str))

  (define (maybe-improper s)
    (define str (string-trim s))
    (cond
      [(regexp-match #px"^[\\(\\[\\{](.*)[\\)\\]\\}]$" str) => second]
      [else (string-append ". " str)]))

  (define (->text s proper? cvt)
    (define old (cvt s))
    (define orig? (or (syntax-original? s) (gen? s)))
    (match* (orig? proper?)
      [(#t #t) old]
      [(#t #f) (maybe-improper old)]
      [(#f #f) (un-improper old orig?)]
      [(#f #t) (just old orig?)]))

  (define current->text
    (make-parameter
     (λ (s)
       (cond
         [(syntax-property s 'transform)
          (format "~a" (syntax->datum s))]))))

  (define (fmt s proper?)
    (->text s proper? (current->text)))

  (define (gen s)
    (syntax-property (datum->syntax #f s) 'transform #t))
  
  (define (gen? s)
    (syntax-property s 'transform))
  )

(require 'text)

(define (loc stx)
  (cons (syntax-position stx)
        (syntax-span stx)))

(define (flat stx)
  (define l (loc stx))
  (let loop ([stx stx])
    (syntax-case stx ()
      [() '()]
      [(h . t)
       (let ([a (loc #'t)])
         (if (equal? a l) 
             (cons #'h (flat #'t)) 
             (list #'h #'t)))]
      [_ (list stx)])))

(define (relocate ls)
  (cond
    [(null? ls) (values 1 0)]
    [else
     (define-values (a b)
       (for/fold ([a +inf.0]
                  [b 0])
                 ([stx (in-list ls)])
         (define p (syntax-position stx))
         (define s (syntax-span stx))
         (values (min a p)
                 (max b (+ p s)))))
     (define ea (inexact->exact a))
     (values ea (- b ea))]))

(define (join lss sep)
  (if (null? lss)
      '()
      (append (car lss)
              (let loop ([lss (cdr lss)])
                (if (null? lss)
                    '()
                    (cons sep (append (car lss) (loop (cdr lss)))))))))

(begin-for-syntax

  (define (get-sep l r)
    (if (< l r) #''("\n") #''()))

  (define (convert-list ls [line 1])
    (syntax-parse ls
      [()
       (values #''("") 0)]
      [((~datum unquote) a:expr)
       (values #'(list (fmt a #f)) (or (syntax-line #'a) line))]
      [((~and here ((~datum ...@) . h)) (~and me ((~datum ....) [x:id e:expr] ...)) . t)
       #:do [(define-values (hs hl) (convert-list #'h (syntax-line #'here)))
             (define-values (ts tl) (convert-list #'t hl))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (define ss
         (if (< hl (syntax-line #'me))
             #'(let ([l (filter
                         non-empty-string?
                         (join
                          (for/list ([x (in-list (syntax->list e))] ...)
                            ht)
                          '"\n"))])
                 (if (null? l)
                     tt
                     (cons
                      (string-join l)
                      (append sep tt))))
             #'(let ([l (filter
                         non-empty-string?
                         (apply append
                                (for/list ([x (in-list (syntax->list e))]
                                           ...)
                                  ht)))])
                 (if (null? l)
                     tt
                     (append l sep tt)))))
       (values ss hl)]
      [(h (~and me ((~datum ....) [x:id e:expr] ...)) . t)
       #:do [(define-values (hs hl) (convert #'h line))
             (define-values (ts tl) (convert-list #'t))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values
        (if (< hl (syntax-line #'me))
            #'(cons
               (string-join
                (filter
                 non-empty-string?
                 (for/list ([x (in-list (syntax->list e))] ...)
                   ht))
                "\n")
               (append sep tt))
            #'(append
               (for/list ([x (in-list (syntax->list e))] ...)
                 ht)
               sep
               tt))
        hl)]
      [(h . t)
       #:do [(define-values (hs hl) (convert #'h line))
             (define-values (ts tl) (convert-list #'t hl))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values #'(cons ht (append sep tt)) hl)]))

  (define (convert stx [line 1])
    (syntax-parse stx
      [((~datum unquote) a:expr)
       (values #'(fmt a #t) (or (syntax-line stx) line))]
      [((~or (~and (~datum syntax) (~bind [p #''"#'"]))
             (~and (~datum quote) (~bind [p #''"'"]))) form)
       #:do [(define-values (fs fl) (convert #'form line))]
       #:with f fs
       (values #'(string-append p f) (syntax-line stx))]
      [()
       (values #''"()" (syntax-line stx))]
      [(~and l (a . b))
       #:do [(define lshape (or (syntax-property #'l 'paren-shape) #\())
             (define rshape (case lshape
                              [(#\() #\)]
                              [(#\[) #\]]
                              [(#\{) #\}]))
             (define-values (t _) (convert-list #'(a . b) line))]
       #:with t t
       (values
        #`(string-append
           (~a #,lshape)
           (string-join (filter non-empty-string? t))
           (~a #,rshape))
        (syntax-line stx))]
      [thing (values #`'#,(format "~a" (syntax->datum stx))
                     (syntax-line stx))]))

  )

(define-syntax-rule (cases obj clauses ...)
  (let ([it obj])
    (syntax-parse it
      clauses
      [_ (void)])
    ...))

(define-syntax (~ stx)
  (syntax-case stx ()
    [(_ t)
     (let-values ([(t l) (convert #'t)])
       t)]))

(define (append-options add str ed ev)
  (define (->text s)
    (cond
      [(syntax-property s 'transform)
       (format "~a" (syntax->datum s))]
      [(syntax-original? s) 
       (define start (syntax-position s))
       (define span (syntax-span s))
       (define beg (+ start -1))
       (define str* (substring str beg (+ beg span))) 
       str*] 
      [else  
       (define-values (start span) (relocate (flat s)))
       (define beg (+ start -1))
       (define str* (substring str beg (+ beg span))) 
       str*]))

  (parameterize ([current->text ->text])
    (with-handlers ([exn:fail:read? void])
      (define p (open-input-string str))
      (port-count-lines! p)
      (define stx (read-syntax #f p))
              
      (syntax-parse stx
        [((~datum if) test then else)
         (add "to cond"
              (if (and (> (syntax-line #'then) (syntax-line #'test))
                       (> (syntax-span #'test) 30))
                  (~ (cond
                       [,#'test
                        ,#'then]
                       [else
                        ,#'else]))
                  (~ (cond
                       [,#'test ,#'then]
                       [else ,#'else]))))]
        [((~datum cond) ~! . _)
         (cases stx
           [(_ [test then] [(~datum else) else])
            (add "to if"
                 (if (= (syntax-line #'then) (syntax-line #'else))
                     (~ (if ,#'test ,#'then ,#'else))
                     (~ (if ,#'test
                            ,#'then
                            ,#'else))))]
           [(_ [test . then] [(~datum else) . else])
            (add "name test"
                 (~ (cond
                      [,#'test
                       =>
                       (λ (it)
                         . ,#'then)]
                      [else
                       . ,#'else])))]
           [(_
             [test (~datum =>) ((~or (~datum lambda) (~datum λ)) (it) then)]
             [(~datum else) else])
            (add "to if"
                 (~ (let ([it ,#'test])
                      (if it
                          ,#'then
                          ,#'else))))])]
        [((~datum define) . t)
         (cases stx
           [(_ (name . args) . e)
            (add "expand with let"
                 (~ (define ,#'name
                      (let ()
                        (λ ,#'args
                          . ,#'e)))))
            (add "expand"
                 (~ (define ,#'name
                      (λ ,#'args
                        . ,#'e))))]
           [(_ x:id e:expr)
            (add "to define-values"
                 (if (= (syntax-line #'x) (syntax-line #'e))
                     (~ (define-values (,#'x) ,#'e))
                     (~ (define-values (,#'x)
                          ,#'e))))]
           [(_ head (~or ((~or (~datum λ) (~datum lambda)) args . body)
                         ((~or (~datum let) (~datum letrec))
                          ()
                          ((~or (~datum λ) (~datum lambda)) args . body))))
            (add "to function notation"
                 (~ (define (,#'head . ,#'args)
                      . ,#'body)))])]
        [((~datum define-values) (x:id) e:expr)
         (add "to define"
              (if (= (syntax-line #'x) (syntax-line #'e))
                  (~ (define ,#'x ,#'e))
                  (~ (define ,#'x
                       ,#'e))))]
        [((~datum define-syntax-rule) (name . pat) body)
         (add "to syntax-rules"
              (~ (define-syntax ,#'name
                   (syntax-rules ()
                     [(_ . ,#'pat)
                      ,#'body]))))
         (add "to syntax-case"
              (~ (define-syntax (,#'name stx)
                   (syntax-case stx ()
                     [(_ . ,#'pat)
                      #',#'body]))))]
        [((~datum define-syntax) (name:id param:id) body)
         (cases #'body
           [((~datum syntax-case) param2 () [(_ . pat) ((~datum syntax) tpl)])
            #:when (free-identifier=? #'param2 #'param)
            (add "to define-syntax-rule"
                 (~ (define-syntax-rule (,#'name . ,#'pat)
                      ,#'tpl)))]
           [((~datum syntax-case) param2 lit [(_ . pat) ((~datum syntax) tpl)] ...)
            #:when (free-identifier=? #'param2 #'param)
            (add "to syntax-rules"
                 (~ (define-syntax ,#'name
                      (syntax-rules ,#'lit
                        [(_ . ,p)
                         ,t]
                        (.... [p #'(pat ...)] [t #'(tpl ...)])))))])]
        [((~datum define-syntax) name:id body)
         (cases #'body
           [((~datum syntax-rules) () [(_ . pat) tpl])
            (add "to define-syntax-rule"
                 (~ (define-syntax-rule (,#'name . ,#'pat)
                      ,#'tpl)))]
           [((~datum syntax-rules) lit [(_ . pat) tpl] ...)
            (add "to syntax-case"
                 (~ (define-syntax (,#'name stx)
                      (syntax-case stx ,#'lit
                        [(_ . ,p)
                         #',t]
                        (.... [p #'(pat ...)] [t #'(tpl ...)])))))])]
        [((~datum syntax) pat)
         (add "to syntax/loc"
              (if (< (syntax-span #'pat) 30)
                  (~ (syntax/loc #'k ,#'pat))
                  (~ (syntax/loc #'k
                       ,#'pat))))]
        [((~datum quasisyntax) pat)
         (add "to quasisyntax/loc"
              (if (< (syntax-span #'pat) 30)
                  (~ (quasisyntax/loc #'k ,#'pat))
                  (~ (quasisyntax/loc #'k
                       ,#'pat))))]
        [((~datum quote) (data ...))
         (add "to (list ...)"
              (~ (list (quote ,d) (.... [d #'(data ...)]))))]
        [((~and h (~or (~datum let) (~datum let*))) . _)
         #:with lv (gen (syntax-parse #'h
                          [(~datum let) 'let-values]
                          [else 'let*-values]))
         (cases stx
           [(_ ([id expr] ...) . body)
            (add "to let-values"
                 (~ (,#'lv ([(,id) ,expr]
                            (.... [id #'(id ...)] [expr #'(expr ...)]))
                           . ,#'body)))
            (add "to defines"
                 (~ (begin
                      (define ,id ,expr)
                      (.... [id #'(id ...)] [expr #'(expr ...)])
                      . ,#'body)))]
           [(_ ([id expr] ...) ((~or (~datum λ) (~datum lambda)) args . body))
            #:with (keys ...)
            (for/list ([id (in-syntax #'(id ...))])
              (gen (string->keyword (symbol->string (syntax-e id)))))
            (add "let as keywords"
                 (~ (λ ((...@ ,k [,id ,e])
                        (.... [k #'(keys ...)] [id #'(id ...)] [e #'(expr ...)])
                        . ,#'args)
                      . ,#'body)))])]
        [((~and h (~or (~datum let-values) (~datum let*-values))) . _)
         #:with lv (gen (syntax-parse #'h
                          [(~datum let-values) 'let]
                          [else 'let*]))
         (cases stx
           [(_  ([(id) expr] ...) . body)
            (add "to let"
                 (~ (,#'lv ([,id ,expr]
                            (.... [id #'(id ...)] [expr #'(expr ...)]))
                           . ,#'body)))]
           [(_ ([id expr] ...) . body)
            (add "to define-values"
                 (~ (begin
                      (define-values ,id ,expr)
                      (.... [id #'(id ...)] [expr #'(expr ...)])
                      . ,#'body)))])]
        [_ (void)])))
  (void))

(provide append-options)