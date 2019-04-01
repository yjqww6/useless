#lang racket/base
(require (for-syntax racket/base racket/sequence racket/syntax
                     syntax/parse syntax/id-table syntax/id-set)
         racket/stxparam racket/splicing racket/class racket/gui/base)

(define-syntax (define-gui stx)
  (syntax-parse stx
    [(_ (~and defs ([x:id %:expr [f:id e:expr] ...] ...))
        (~and edges ([p:id c:id ...] ...)))
     (define table (make-free-id-table))
     (define parent-table (make-free-id-table))
     (for ([def (in-syntax #'defs)])
       (syntax-parse def
         [[x:id %:expr [f:id e:expr] ...]
          (free-id-table-set! table #'x (cons #'% #'([f e] ...)))]))

     (for ([edge (in-syntax #'edges)])
       (syntax-parse edge
         [[p:id c:id ...]
          (for ([c (in-syntax #'(c ...))])
            (free-id-table-set! parent-table c #'p))]))

     (define oked (mutable-free-id-set))
     (define emited '())

     (define (emit x f p)
       (unless (free-id-set-member? oked x)
         (cond
           [p
            (define pf (free-id-table-ref table p))
            (define pp (free-id-table-ref parent-table p (λ () #f)))
            (emit p pf pp)
            (with-syntax ([x x] [p p] [(% . f) f])
              (set! emited (cons #'(define x (new % [parent p] . f))
                                 emited)))]
           [else
            (with-syntax ([x x] [(% . f) f])
              (set! emited (cons #'(define x (new % . f))
                                 emited)))])
         (free-id-set-add! oked x)))
     
     (for ([(k v) (in-free-id-table table)])
       (define p (free-id-table-ref parent-table k (λ () #f)))
       (emit k v p))
     (with-syntax ([(defs ...) (reverse emited)])
       #'(begin defs ...))
     ]))

(define-syntax-parameter parent #f)

(define-syntax (: stx)
  (syntax-parse stx
    [(_ (~optional x:id
                   #:defaults ([x (generate-temporary '_)]))
        (% [f:id e:expr] ...)
        child ...)
     #:with (p ...)
     (cond
       [(syntax-parameter-value #'parent)
        =>
        (λ (it)
          #`([parent #,it]))]
       [else #'()])
     #'(begin
         (define x (new % p ... [f e] ...))
         (splicing-syntax-parameterize ([parent #'x])
           child ...))]
    [(_ e:expr (~literal :) child:expr ...)
     #'(begin
         (define x e)
         (splicing-syntax-parameterize ([parent #'x])
           child ...))]))

(provide (all-defined-out))

(module+ test
  (require rackunit)
  (: f (frame% [label "test"])
     (: (horizontal-pane%)
        (: (button% [label "test1"]))
        (: (button% [label "test2"])))
     (: (vertical-pane%)
        (: (button% [label "test1"]))
        (: b4 (button% [label "b4"]))))

  (check-equal? (send b4 get-label) "b4")


  (define-gui
    ([fr frame% [label "test"]]
     [btn button% [label "test"]]
     [btn2 button% [label "test"]]
     [btn3 button% [label "test"]]
     [btn4 button% [label "btn4"]]
   
     [h horizontal-pane%]
     [v vertical-pane%])
    ([fr v h]
     [v btn btn4]
     [h btn2 btn3]))

  (check-equal? (send btn4 get-label) "btn4"))
