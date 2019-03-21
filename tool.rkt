#lang racket
(require racket/gui drracket/tool framework syntax/parse
         "and-body.rkt" "scope-guard.rkt"
         (for-syntax syntax/parse))

(provide tool@)

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
    (define orig? (syntax-original? s))
    (match* (orig? proper?)
      [(#t #t) old]
      [(#t #f) (maybe-improper old)]
      [(#f #f) (un-improper old orig?)]
      [(#f #t) (just old orig?)]))

  (define current->text (make-parameter #f))

  (define (fmt s proper?)
    (->text s proper? (current->text))))

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
    [(null? ls) (values 0 0)]
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

(begin-for-syntax

  (define (get-sep l r)
    (if (< l r) #''("\n") #''()))

  (define (convert-list ls)
    (syntax-parse ls
      [()
       (values #''("") 0)]
      [((~datum unquote) a:expr)
       (values #'(list (fmt a #f)) (syntax-line #'a))]
      [(h (~and me ((~datum ....) [x:id e:expr] ...)) . t)
       #:do [(define-values (hs hl) (convert #'h))
             (define-values (ts tl) (convert-list #'t))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values
        (if (< hl (syntax-line #'me))
            #'(cons
               (string-join
                (for/list ([x (in-list (syntax->list e))] ...)
                  ht)
                "\n")
               (append sep tt))
            #'(append
               (for/list ([x (in-list (syntax->list e))] ...)
                 ht)
               sep
               tt))
        hl)]
      [(h . t)
       #:do [(define-values (hs hl) (convert #'h))
             (define-values (ts tl) (convert-list #'t))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values #'(cons ht (append sep tt)) hl)]))

  (define (convert stx)
    (syntax-parse stx
      [((~datum unquote) a:expr)
       (values #'(fmt a #t) (syntax-line stx))]
      [((~datum syntax) form)
       #:do [(define-values (fs fl) (convert #'form))]
       #:with f fs
       (values #'(string-append '"#'" f) (syntax-line stx))]
      [()
       (values #''"()" (syntax-line stx))]
      [(~and l (a . b))
       #:do [(define lshape (or (syntax-property #'l 'paren-shape) #\())
             (define rshape (case lshape
                              [(#\() #\)]
                              [(#\[) #\]]
                              [(#\{) #\}]))
             (define-values (t _) (convert-list #'(a . b)))]
       #:with t t
       (values
        #`(string-append
           (~a #,lshape)
           (string-join (filter non-empty-string? t))
           (~a #,rshape))
        (syntax-line stx))]
      [thing (values #`'#,(format "~a" (syntax->datum stx))
                     (syntax-line stx))])))

(define-syntax (~ stx)
  (syntax-case stx ()
    [(_ t)
     (let-values ([(t l) (convert #'t)])
       t)]))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (keymap:add-to-right-button-menu
     (let ([orig (keymap:add-to-right-button-menu)])
       (λ (menu ed ev)
         
         (orig menu ed ev)
         
         (define pos (send ed get-start-position))
         
         (when (and pos (is-a? ed drracket:unit:definitions-text<%>))
           
           (and-body
            (define end (send ed get-forward-sexp pos))
            (define str (send ed get-text pos end))
            
            (define (->text s)
              (if (syntax-original? s)
                  (let* ([start (syntax-position s)]
                         [span (syntax-span s)]
                         [beg (+ start -1)]
                         [str (substring str beg (+ beg span))])
                    str)
                  (let*-values ([(start span) (relocate (flat s))]
                                [(beg) (+ start -1)]
                                [(str) (substring str beg (+ beg span))])
                    str)))

            (parameterize ([current->text ->text])
              (with-handlers ([exn:fail:read? void])
                (define p (open-input-string str))
                (port-count-lines! p)
                (define stx (read-syntax #f p))
              
                (define (add label neo)
                  (new menu-item% [label label] [parent menu]
                       [callback
                        (λ (m e)
                          (with-scope-guard guard
                            (send ed begin-edit-sequence)
                            (guard (send ed end-edit-sequence))
                            (send ed delete pos end)
                            (send ed insert neo pos)
                            (define neo-pos (send ed get-forward-sexp pos))
                            (when neo-pos
                              (send ed tabify-selection pos neo-pos))))]))
              
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
                  [((~datum cond) [test then] [(~datum else) else])
                   (add "to if"
                        (if (= (syntax-line #'then) (syntax-line #'else))
                            (~ (if ,#'test ,#'then ,#'else))
                            (~ (if ,#'test
                                   ,#'then
                                   ,#'else))))]
                  [((~datum define) . t)
                   (syntax-parse stx
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
                     [_ (void)])
                   (syntax-parse stx
                     [(_ x:id e:expr)
                      (add "to define-values"
                           (if (= (syntax-line #'x) (syntax-line #'e))
                               (~ (define-values (,#'x) ,#'e))
                               (~ (define-values (,#'x)
                                    ,#'e))))]
                     [_ (void)])
                   (syntax-parse stx
                     [(_ head (~or ((~or (~datum λ) (~datum lambda)) args . body)
                                   ((~or (~datum let) (~datum letrec))
                                    ()
                                    ((~or (~datum λ) (~datum lambda)) args . body))))
                      (add "to function notation"
                           (~ (define (,#'head . ,#'args)
                                . ,#'body)))]
                     [_ (void)])]
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
                   (syntax-parse #'body
                     [((~datum syntax-case) param2 () [(_ . pat) ((~datum syntax) tpl)])
                      #:when (free-identifier=? #'param2 #'param)
                      (add "to define-syntax-rule"
                           (~ (define-syntax-rule (,#'name . ,#'pat)
                                ,#'tpl)))]
                     [_ (void)])
                   (syntax-parse #'body
                     [((~datum syntax-case) param2 lit [(_ . pat) ((~datum syntax) tpl)] ...)
                      #:when (free-identifier=? #'param2 #'param)
                      (add "to syntax-rules"
                           (~ (define-syntax ,#'name
                                (syntax-rules ,#'lit
                                  [(_ . ,p)
                                   ,t]
                                  (.... [p #'(pat ...)] [t #'(tpl ...)])))))]
                     [_ (void)])]
                  [((~datum define-syntax) name:id body)
                   (syntax-parse #'body
                     [((~datum syntax-rules) () [(_ . pat) tpl])
                      (add "to define-syntax-rule"
                           (~ (define-syntax-rule (,#'name . ,#'pat)
                                ,#'tpl)))]
                     [_ (void)])
                   (syntax-parse #'body
                     [((~datum syntax-rules) lit [(_ . pat) tpl] ...)
                      (add "to syntax-case"
                           (~ (define-syntax (,#'name stx)
                                (syntax-case stx ,#'lit
                                  [(_ . ,p)
                                   #',t]
                                  (.... [p #'(pat ...)] [t #'(tpl ...)])))))]
                     [_ (void)])]
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
                  [_ (void)])))))
         (void)
         )))))