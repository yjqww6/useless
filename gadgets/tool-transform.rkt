#lang racket

(require racket/gui syntax/parse
         (for-syntax syntax/parse)
         drracket/tool framework)

(require "gadget-sig.rkt" "../and-body.rkt" "../scope-guard.rkt" "../tpl.rkt" "../logger.rkt")
(provide tool@)
(define-unit tool@
  (import drracket:tool^)
  (export gadget^)

  (define (append-here add sep str ed ev)
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

        (cases stx
          [(_ . body)
           (add "unwrap 1"
                (unwrap (~ ,#'body)))]
          [(_ _ . body)
           (add "unwrap 2"
                (unwrap (~ ,#'body)))]
          [_
           (add "wrap with begin"
                (~ (begin
                     ,stx)))
           (add "wrap with let"
                (~ (let ()
                     ,stx)))])

        (sep)
              
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
             [(_ (~and [test . then]
                       [_ (~not (~datum =>)) _])
                 [(~datum else) . else])
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
          [((~datum let) loop:id ([x:id e:expr] ...) . body)
           (add "to letrec"
                (~ (letrec ([,#'loop (λ (,x (.... [x #'(x ...)]))
                                       . ,#'body)])
                     (,#'loop ,e (.... [e #'(e ...)])))))
           (add "to letrec(equivalent)"
                (~ ((letrec ([,#'loop (λ (,x (.... [x #'(x ...)]))
                                        . ,#'body)])
                      ,#'loop)
                    ,e (.... [e #'(e ...)]))))
           (add "to define(begin)"
                (~ (begin
                     (define (,#'loop ,x (.... [x #'(x ...)]))
                       . ,#'body)
                     (,#'loop ,e (.... [e #'(e ...)])))))
           (add "to define(let)"
                (~ (let ()
                     (define (,#'loop ,x (.... [x #'(x ...)]))
                       . ,#'body)
                     (,#'loop ,e (.... [e #'(e ...)])))))
           (add "to define"
                (unwrap
                 (~ ((define (,#'loop ,x (.... [x #'(x ...)]))
                       . ,#'body)
                     (,#'loop ,e (.... [e #'(e ...)]))))))]
          [((~and h (~or (~datum let) (~datum let*))) . _)
           #:with lv (gen (syntax-parse #'h
                            [(~datum let) 'let-values]
                            [else 'let*-values]))
           (cases stx
             [(_ ([x1:id test]) ((~datum if) x2:id then else))
              #:when (free-identifier=? #'x1 #'x2)
              (add "to cond"
                   (if (> (syntax-line #'then) (syntax-line #'x2))
                       (~ (cond
                            [,#'test
                             =>
                             (λ (,#'x1)
                               ,#'then)]
                            [else
                             ,#'else]))
                       (~ (cond
                            [,#'test => (λ (,#'x1) ,#'then)]
                            [else ,#'else]))))]
             [(_ ([id expr] ...) . body)
              (add "to let-values"
                   (~ (,#'lv ([(,id) ,expr]
                              (.... [id #'(id ...)] [expr #'(expr ...)]))
                             . ,#'body)))
              (add "to defines(begin)"
                   (~ (begin
                        (define ,id ,expr)
                        (.... [id #'(id ...)] [expr #'(expr ...)])
                        . ,#'body)))
              (add "to defines(let)"
                   (~ (let ()
                        (define ,id ,expr)
                        (.... [id #'(id ...)] [expr #'(expr ...)])
                        . ,#'body)))
              (add "to defines(unwrap)"
                   (unwrap
                    (~ ((define ,id ,expr)
                        (.... [id #'(id ...)] [expr #'(expr ...)])
                        . ,#'body))))]
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
              (add "to define-values(begin)"
                   (~ (begin
                        (define-values ,id ,expr)
                        (.... [id #'(id ...)] [expr #'(expr ...)])
                        . ,#'body)))
              (add "to define-values(let)"
                   (~ (let ()
                        (define-values ,id ,expr)
                        (.... [id #'(id ...)] [expr #'(expr ...)])
                        . ,#'body)))
              (add "to define-values(unwrap)"
                   (unwrap
                    (~ ((define-values ,id ,expr)
                        (.... [id #'(id ...)] [expr #'(expr ...)])
                        . ,#'body))))])]
          [_ (void)]))))

  (define (append-options menu ed ev)
    (new separator-menu-item% [parent menu])
    (when (is-a? ed racket:text<%>)
      (new menu-item% [label "let and print"] [parent menu]
           [callback (λ (m e)
                       (send ed introduce-let-ans
                             (send ed get-start-position)))])
      (new menu-item% [label "transpose"] [parent menu]
           [callback (λ (m e)
                       (send ed transpose-sexp
                             (send ed get-start-position)))])
      (new separator-menu-item% [parent menu]))
    (define-values (_ t1 t2 t3)
      (time-apply
       (λ ()
         (and-body
          #:all
          (is-a? ed drracket:unit:definitions-text<%>)
          (define pos (send ed get-start-position))
          (define end (send ed get-forward-sexp pos))
          (define str (send ed get-text pos end))
              
          (define (add label neo)
            (new menu-item% [label label] [parent menu]
                 [callback
                  (λ (m e)
                    (with-scope-guard guard
                      (send ed begin-edit-sequence)
                      (guard (send ed end-edit-sequence))
                      (send ed delete pos end)
                      (send ed insert neo pos)
                      (define neo-pos (+ pos (string-length neo)))
                      (when neo-pos
                        (send ed tabify-selection pos neo-pos))))]))

          (define (sep)
            (new separator-menu-item% [parent menu]))

          (append-here add sep str ed ev)))
       '()))
    (log-useless-debug "append-options called in ~ams" t1)
    (void))

  (define gadgets (hash 'popup append-options)))