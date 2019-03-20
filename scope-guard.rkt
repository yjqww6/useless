#lang racket/base

(require (for-syntax racket/base
                     syntax/parse syntax/context racket/syntax)
         syntax/parse/define)
(provide with-defer with-scope-guard)

(begin-for-syntax
  (define-splicing-syntax-class prop
    #:attributes (b e)
    (pattern (~seq (~alt (~optional (~seq #:barrier? b:boolean)
                                    #:defaults ([b #'#t]))
                         (~optional (~seq #:handle-exception-only? e:boolean)
                                    #:defaults ([e #'#f])))
                   ...)))

  (define (handle-prop body done hole b? e?)
    (define e
      (hole
       (if e?
           #`(begin0
               (call-with-exception-handler
                (λ (e) #,done e)
                (λ () #,@body))
               #,done)
           #`(dynamic-wind
              void
              (λ () #,@body)
              (λ () #,done)))))
    (if b?
        #`(call-with-continuation-barrier
           (λ () #,e))
        e)))

(define-syntax-parser with-defer
  [(_ ?defer:id ?prop:prop
      ?body:expr ...+)
   (handle-prop #'(?body ...)
                #'(done)
                (λ (e)
                  #`(let ()
                      (define defers '())
                      (define (done)
                        (for ([d (in-list defers)]) (d)))
                      (define-simple-macro (?defer ?expr:expr ...+)
                        (set! defers (cons (λ () ?expr (... ...)) defers)))
                      #,e))
                (syntax-e #'?prop.b)
                (syntax-e #'?prop.e))])

(begin-for-syntax
  (define (here-expand* ctx stx* defered guard)
    (datum->syntax
     stx*
     (for/list ([s (in-list (syntax->list stx*))])
       (here-expand ctx s defered guard))))
  
  (define orig-insp
    (variable-reference->module-declaration-inspector
     (#%variable-reference)))

  (define (here-expand ctx stx defered guard)
    (define e
      (local-expand stx (generate-expand-context)
                    (list #'define-values #'define-syntaxes #'begin guard)
                    ctx))
    (syntax-rearm
     (syntax-parse (syntax-disarm e orig-insp)
       [((~literal define-values) (id:id ...) body:expr)
        (define ids (syntax->list #'(id ...)))
        (syntax-local-bind-syntaxes ids #f ctx)
        #`(define-values #,(map syntax-local-identifier-as-binding ids)
            body)]
       [((~literal define-syntaxes) (id:id ...) body:expr)
        (define ids (syntax->list #'(id ...)))
        (with-syntax ([rhs (local-transformer-expand #'body 'expression '())])
          (syntax-local-bind-syntaxes ids #'rhs ctx)
          #`(define-syntaxes
              #,(map syntax-local-identifier-as-binding ids)
              rhs))]
       [((~literal begin) body:expr ...)
        #`(begin #,@(here-expand* ctx #'(body ...) defered guard))]
       [(head:id expr:expr ...)
        #:when (free-identifier=? #'head guard)
        (record-disappeared-uses #'head)
        (define expanded
          (local-expand (syntax-rearm #'(begin expr ...) e)
                        'expression '() ctx))
        (defered expanded)]
       [e #'e])
     e)))

(define-syntax (with-scope-guard stx)
  (syntax-parse stx
    [(_ ?guard:id ?prop:prop
        ?body:expr ...)
     (define intdef-ctx (syntax-local-make-definition-context))

     (define esc (generate-temporary #'esc))

     (define final (generate-temporary 'd))

     (define entry (list (cons final #'(void))))
       
     (define (defered e)
       (define me (generate-temporary 'd))
       (set! entry (cons (cons me e) entry))
       #`(set! #,esc #,me))

     (syntax-property
      (with-disappeared-uses
          (define body
            (here-expand* intdef-ctx #'(?body ...) defered #'?guard))

        (define bindings
          (let loop ([entry entry])
            (cond
              [(eq? (caar entry) final) (list (list final #'(λ () (void))))]
              [else (cons
                     (list (caar entry) #`(λ () #,(cdar entry) (#,(caadr entry))))
                     (loop (cdr entry)))])))
     
        (handle-prop #`((define-syntax (?guard stx)
                          (raise-syntax-error
                           (syntax-e #'?guard)
                           "not in immediate definition context"
                           stx))
                        #,@(map (λ (stx) (cons #'define stx)) bindings)
                        #,@body)
                  
                     #`(#,esc)
                     (λ (e) #`(let ([#,esc void]) #,e))
                     (syntax-e #'?prop.b)
                     (syntax-e #'?prop.e)))
      'disappeared-binding
      (list (syntax-local-introduce #'?guard)))]))