#lang racket
(require racket/gui drracket/tool framework racket/runtime-path
         compiler/cm "methods.rkt"
         "gadgets/gadget-sig.rkt"
         "logger.rkt" "gadgets/default.rkt")

(provide tool@)

(define-runtime-path gadgets/ "gadgets")

(define-syntax-rule (first e)
  (call-with-values
   (λ () e)
   (λ (a . _) a)))

(define (attach!)
  (define ns (variable-reference->namespace (#%variable-reference)))
  (for ([mod (in-list modules)])
    (parameterize ([current-namespace ns])
      (namespace-require mod))
    (namespace-attach-module ns mod)))

(define gadgets+unit (make-hash))

(define gadget-rx #rx"^tool-(.*)\\.rkt$")

(define (load! . gadgets) 
  (with-time "loading gadgets"
    (parameterize ([current-namespace (make-empty-namespace)])
      (attach!)
      (for ([gadget (in-list gadgets)])
        (define tool
          (dynamic-require (build-path gadgets/ gadget) 'tool@))
        (hash-set! gadgets+unit gadget tool)
        (for ([v (in-weak-hash-values reload-observers)])
          (v gadget tool))
        (for ([v (in-weak-hash-values reload-observers)])
          (v))))))
  
(define (unload! . gadgets)
  (for ([gadget (in-list gadgets)])
    (hash-remove! gadgets+unit gadget)
    (for ([v (in-weak-hash-values reload-observers)])
      (v gadget)))
  (for ([v (in-weak-hash-values reload-observers)])
    (v)))

(define (reload-all!)
  (apply load!
         (for/list ([gadget (in-list (sort (directory-list gadgets/) path<?))]
                    #:when (regexp-match? gadget-rx (path->string gadget))
                    #:when (hash-ref gadgets+unit gadget (λ () #f)))
           gadget)))

(define (unload-all!)
  (apply unload!
         (for/list ([gadget (in-list (sort (hash-keys gadgets+unit) path<?))])
           gadget)))

(define (->label filename)
  (cond
    [(regexp-match gadget-rx (path->string filename))
     => second]
    [else filename]))

(define (refresh-menu add sep)
  (define-values (r l)
    (for/fold ([r '()] [l '()])
              ([gadget (in-list (sort (directory-list gadgets/) path<?))]
               #:when (regexp-match? gadget-rx (path->string gadget)))
      (cond
        [(hash-ref gadgets+unit gadget (λ () #f))
         (values (cons (λ () (add (format "reload ~a" (->label gadget)) (λ () (load! gadget)))) r)
                 l)]
        [else
         (values r (cons (λ () (add (format "load ~a" (->label gadget)) (λ () (load! gadget)))) l))])))
  (for ([r (in-list r)]) (r))
  (sep)
  (for ([r (in-list l)]) (r))
  (sep)
  (for ([gadget (in-list (sort (hash-keys gadgets+unit) path<?))])
    (add (format "unload ~a" (->label gadget)) (λ () (unload! gadget)))))

(define reload-observers (make-weak-hasheq))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define frame-mixin
      (mixin (frame:standard-menus<%>) ()
        (super-new)
        
        (define menu-bar (send this get-menu-bar))

        (define/private (refresh-demand)
          (for ([item (in-list (send useless-menu get-items))]
                #:unless (memq item persistent))
            (send item delete))
          (refresh-menu
           (λ (str cb)
             (new menu-item% [parent useless-menu]
                  [label str] [callback (λ (a b) (cb) (refresh-demand))]))
           (λ ()
             (new separator-menu-item%
                  [parent useless-menu]))))
        
        (define useless-menu
          (new menu% [parent menu-bar] [label "&Useless"]))

        (define persistent
          (list
           (new menu-item% [parent useless-menu]
                [label "Recompiling Gadgets"]
                [callback (λ (m e)
                            (with-time "Recompiling Gadgets"
                              (for ([gadget (in-list (sort (directory-list gadgets/) path<?))]
                                    #:when (regexp-match? gadget-rx (path->string gadget)))
                                (managed-compile-zo (build-path gadgets/ gadget)))))])
           (new menu-item% [parent useless-menu]
                [label "Refresh Gadgets"]
                [callback (λ (m e)
                            (refresh-demand))])
           (new menu-item% [parent useless-menu]
                [label "Reload all Gadgets"]
                [callback (λ (m e)
                            (reload-all!)
                            (refresh-demand))])
           (new menu-item% [parent useless-menu]
                [label "Unload all Gadgets"]
                [callback (λ (m e)
                            (unload-all!)
                            (refresh-demand))])
           (new separator-menu-item%
                [parent useless-menu])))

        (refresh-demand)))

    (define unit-instances (make-hash))

    (define unit-observers (make-weak-hash))

    (define tool-ns (current-namespace))

    (define unit-obserser
      (case-lambda
        [()
         (for ([v (in-hash-values unit-observers)])
           (v))]
        [(path)
         (hash-remove! unit-instances path)
         (for ([v (in-hash-values unit-observers)])
           (v path))]
        [(path tool)
         (define-values/invoke-unit tool
           (import drracket:tool^)
           (export gadget^))
         (hash-set! unit-instances path gadgets)
         (for ([v (in-hash-values unit-observers)])
           (v path gadgets))]))
    
    (hash-set! reload-observers unit-instances unit-obserser)

    (define (make-sur sur% surrogate-compose)
      (define (observer instances key set-sur!)
        (define (compose-gadgets insts)
          (define surs (map cdr (sort (hash->list insts) path<? #:key car)))
          (foldr surrogate-compose (car surs) (cdr surs)))
        (case-lambda
          [()
           (set-sur! (if (hash-empty? instances)
                         #f
                         (compose-gadgets instances)))]
          [(path)
           (hash-remove! instances path)]
          [(path gadgets)
           (cond
             [(hash-ref gadgets key (λ () #f))
              =>
              (λ (it)
                (hash-set! instances path (new (it sur%))))]
             [else
              (hash-remove! instances path)])]))

      (define (load-already key set-sur!)
        (define instances (make-hash))
        (define ob (observer instances key set-sur!))
        (for ([(path gadgets) (in-hash unit-instances)])
          (ob path gadgets))
        (ob)
        instances)
      (values observer load-already))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)

    (define ((text-mixin key) %)
      (define-values (observer load-already) (make-sur surrogate% surrogate-compose))
      (class (host-mixin %)
        (inherit set-private-surrogate invalidate-bitmap-cache)
        (super-new)
        (hash-set! unit-observers this
                   (let* ([set-sur! (λ (sur)
                                      (set-private-surrogate sur)
                                      (invalidate-bitmap-cache))]
                          [instances (load-already key set-sur!)])
                     (observer instances key set-sur!)))))

    (drracket:get/extend:extend-definitions-text
     (text-mixin 'definition-mixin))

    (drracket:get/extend:extend-interactions-text
     (text-mixin 'interaction-mixin))

    (define ((canvas-mixin key) %)
      (define-values (observer load-already) (make-sur c:surrogate% c:surrogate-compose))
      (class (c:host-mixin %)
        (inherit set-private-surrogate)
        (super-new)
        (hash-set! unit-observers this
                   (let* ([set-sur! (λ (sur)
                                      (set-private-surrogate sur))]
                          [instances (load-already key set-sur!)])
                     (observer instances key set-sur!)))))

    (drracket:get/extend:extend-definitions-canvas
     (canvas-mixin 'definition-canvas-mixin))

    (drracket:get/extend:extend-interactions-canvas
     (canvas-mixin 'interaction-canvas-mixin))

    (define popup-instances (make-hash))
    (define append-here void)
    (define (compose-popup)
      (define procs (map cdr (sort (hash->list popup-instances) path<? #:key car)))
      (λ (menu ed evt)
        (for ([proc (in-list procs)])
          (proc menu ed evt))))

    (define popup-observer
      (case-lambda
        [()
         (set! append-here (compose-popup))]
        [(path)
         (hash-remove! popup-instances path)]
        [(path gadgets)
         (cond
           [(hash-ref gadgets 'popup (λ () #f))
            =>
            (λ (it)
              (hash-set! popup-instances path it))]
           [else
            (hash-remove! popup-instances path)])]))

    (hash-set! unit-observers popup-instances popup-observer)

    (keymap:add-to-right-button-menu
     (let ([orig (keymap:add-to-right-button-menu)])
       (λ (menu ed ev)
         (orig menu ed ev)
         (append-here menu ed ev))))

    (for ([(path tool) (in-hash gadgets+unit)])
      (unit-obserser path tool))
    (unit-obserser)))

(apply load! defaults)
