#lang racket/base
(require syntax/parse/define
         (for-syntax racket/syntax racket/base syntax/context syntax/parse))

(begin-for-syntax
  (define orig-insp
    (variable-reference->module-declaration-inspector
     (#%variable-reference)))
  (define stops (syntax-e (quote-syntax
                           (begin define-syntaxes define-values))))
  (define-syntax-class (norm intdef-ctx ctx escape all?)
    (pattern
     form:expr
     #:do [(define expanded
             (local-expand (syntax-disarm #'form orig-insp)
                           ctx stops intdef-ctx))]
     #:with k escape
     #:with norm
     (syntax-rearm
      (syntax-parse expanded 
        #:literals (define-values define-syntaxes begin)
        [(begin form* ...)
         #:declare form* (norm intdef-ctx ctx escape all?)
         (syntax/loc #'form (begin form*.norm ...))]
        [(define-values (x:id ...) e:expr)
         #:do [(define ids (syntax->list #'(x ...)))
               (syntax-local-bind-syntaxes ids #f intdef-ctx)]
         #:with (ids ...) (map syntax-local-identifier-as-binding ids)
         (syntax/loc #'form
           (begin (define-values (ids ...) e)
                  (define-values ()
                    (begin
                      (unless (and x ...)
                        (k #f))
                      (values)))))]
        [(define-syntaxes (x:id ...) e:expr)
         #:do [(define ids (syntax->list #'(x ...)))
               (define rhs (local-transformer-expand #'e 'expression '()))
               (syntax-local-bind-syntaxes ids rhs intdef-ctx)]
         #:with (ids ...) (map syntax-local-identifier-as-binding ids)
         #:with rhs rhs
         (syntax/loc #'form
           (define-syntaxes (ids ...) rhs))]
        [sth
         #:with it (generate-temporary 'tmp)
         (if all?
             (syntax/loc #'form
               (let ([it sth])
                 (unless it (k #f))
                 it))
             #'sth)])
      #'form))))

(define-syntax (and-body stx)
  (define intdef-ctx (syntax-local-make-definition-context))
  (define ctx (generate-expand-context))
  (define k (generate-temporary 'k))
  (syntax-parse stx
    [(_ (~optional (~and all #:all) #:defaults ([all #'#f])) form ...+)
     #:declare form (norm intdef-ctx ctx k (syntax-e #'all))
     #:with k k
     #'(let/ec k
         form.norm ...)]))

(provide and-body)