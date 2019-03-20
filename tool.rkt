#lang racket
(require racket/gui drracket/tool framework syntax/parse
         "and-body.rkt" "scope-guard.rkt"
         (for-syntax syntax/parse))

(provide tool@)

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
      [(h (~and me ((~datum ooo) x:id ...)) t ...)
       #:do [(define-values (hs hl) (convert #'h))
             (define-values (ts tl) (convert-list #'(t ...)))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values
        (if (< hl (syntax-line #'me))
            #'(cons
               (string-join
                (for/list ([x (in-list x)] ...)
                  ht)
                "\n")
               (append sep tt))
            #'(append
               (for/list ([x (in-list x)] ...)
                 ht)
               sep
               tt))
        hl)]
      [(h t ...)
       #:do [(define-values (hs hl) (convert #'h))
             (define-values (ts tl) (convert-list #'(t ...)))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values #'(cons ht (append sep tt)) hl)]))

  (define (convert stx)
    (syntax-parse stx
      [((~datum unquote) a:expr)
       (values #'(~a a) (syntax-line stx))]
      [((~datum syntax) form)
       #:do [(define-values (fs fl) (convert #'form))]
       #:with f fs
       (values #'(string-append '"#'" f) (syntax-line stx))]
      [()
       (values #''"()" (syntax-line stx))]
      [(~and l (term ...))
       #:do [(define lshape (or (syntax-property #'l 'paren-shape) #\())
             (define rshape (case lshape
                              [(#\() #\)]
                              [(#\[) #\]]
                              [(#\{) #\}]))
             (define-values (t _) (convert-list #'(term ...)))]
       #:with t t
       (values
        #`(string-append
           (~a #,lshape)
           (string-join (filter non-empty-string? t))
           (~a #,rshape))
        (syntax-line stx))]
      [thing (values #'(~a 'thing) (syntax-line stx))])))

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
              (if (syntax? s)
                  (let* ([start (syntax-position s)]
                         [span (syntax-span s)]
                         [beg (+ start -1)]
                         [str (substring str beg (+ beg span))])
                    str)
                  (let*-values ([(start span) (relocate s)]
                                [(beg) (+ start -1)]
                                [(str) (substring str beg (+ beg span))])
                    str)))
            
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
                               [,(->text #'test)
                                ,(->text #'then)]
                               [else
                                ,(->text #'else)]))
                          (~ (cond
                               [,(->text #'test) ,(->text #'then)]
                               [else ,(->text #'else)]))))]
                [((~datum cond) [test then] [(~datum else) else])
                 (add "to if"
                      (if (= (syntax-line #'then) (syntax-line #'else))
                          (~ (if ,(->text #'test) ,(->text #'then) ,(->text #'else)))
                          (~ (if ,(->text #'test)
                                 ,(->text #'then)
                                 ,(->text #'else)))))]
                [((~datum define-syntax-rule) (~and head (name . _)) body)
                 (let ([a (->text #'name)]
                       [b (->text (cdr (flat #'head)))]
                       [c (->text #'body)])
                   (add "to syntax-rules"
                        (~ (define-syntax ,a
                             (syntax-rules ()
                               [(_ ,b)
                                ,c]))))
                   (add "to syntax-case"
                        (~ (define-syntax (,a stx)
                             (syntax-case stx ()
                               [(_ ,b)
                                #',c])))))]
                [((~datum define-syntax) (name:id param:id) body)
                 (syntax-parse #'body
                   [((~datum syntax-case) param2 () [(~and pat (_ . _)) ((~datum syntax) tpl)])
                    #:when (free-identifier=? #'param2 #'param)
                    (add "to define-syntax-rule"
                         (~ (define-syntax-rule (,(->text #'name) ,(->text (cdr (flat #'pat))))
                              ,(->text #'tpl))))]
                   [_ (void)])
                 (syntax-parse #'body
                   [((~datum syntax-case) param2 lit [(~and pat (_ . _)) ((~datum syntax) tpl)] ...)
                    #:when (free-identifier=? #'param2 #'param)
                    (define pats (map (λ (p) (->text (cdr (flat p)))) (syntax->list #'(pat ...))))
                    (define tpls (map ->text (syntax->list #'(tpl ...))))
                    (add "to syntax-rules"
                         (~ (define-syntax ,(->text #'name)
                              (syntax-rules ,(->text #'lit)
                                [(_ ,pats)
                                 ,tpls]
                                (ooo pats tpls)))))]
                   [_ (void)])]
                [((~datum define-syntax) name:id body)
                 (syntax-parse #'body
                   [((~datum syntax-rules) () [(~and pat (_ . _)) tpl])
                    (add "to define-syntax-rule"
                         (~ (define-syntax-rule (,(->text #'name) ,(->text (cdr (flat #'pat))))
                              ,(->text #'tpl))))]
                   [_ (void)])
                 (syntax-parse #'body
                   [((~datum syntax-rules) lit [(~and pat (_ . _)) tpl] ...)
                    (define pats (map (λ (p) (->text (cdr (flat p)))) (syntax->list #'(pat ...))))
                    (define tpls (map ->text (syntax->list #'(tpl ...))))
                    (add "to syntax-case"
                         (~ (define-syntax (,(->text #'name) stx)
                              (syntax-case stx ,(->text #'lit)
                                [(_ ,pats)
                                 #',tpls]
                                (ooo pats tpls)))))]
                   [_ (void)])]
                [_ (void)]))))
         (void)
         )))))