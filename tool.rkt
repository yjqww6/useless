#lang racket
(require racket/gui drracket/tool framework racket/runtime-path
         "transform-sig.rkt" compiler/cm "methods.rkt" "gadgets/gadget-sig.rkt"
         "logger.rkt"
         (only-in
          (combine-in syntax/parse racket/runtime-config)))

(provide tool@)

(define-runtime-module-path trans-sig.rkt "transform-sig.rkt")
(define-runtime-path trans.rkt "transform.rkt")
(define-runtime-path gadgets/ "gadgets")
(define-runtime-module-path methods.rkt "methods.rkt")
(define-runtime-module-path gadgets/gadget-sig.rkt "gadgets/gadget-sig.rkt")
(define-runtime-module-path logger.rkt "logger.rkt")

(define trans void)

(define-syntax-rule (with-time name body ...)
  (let-values ([(_ t1 t2 t3)
                (time-apply (λ () body ...) '())])
    (log-useless-debug "~a done in ~a ms" name t1)))

(define-syntax-rule (first e)
  (call-with-values
   (λ () e)
   (λ (a . _) a)))

(define (attach!)
  (define ns (variable-reference->namespace (#%variable-reference)))
  (namespace-attach-module ns 'racket)
  (namespace-attach-module ns 'racket/gui)
  (namespace-attach-module ns 'racket/unit)
  (namespace-attach-module ns 'racket/runtime-config)
  (namespace-attach-module ns 'syntax/parse)
  (namespace-attach-module ns 'syntax/parse/define)
  (namespace-attach-module ns 'framework)
  (namespace-attach-module ns 'drracket/tool)
  (namespace-attach-module ns trans-sig.rkt)
  (namespace-attach-module ns methods.rkt)
  (namespace-attach-module ns gadgets/gadget-sig.rkt)
  (namespace-attach-module ns logger.rkt))

(define (reload-transform [first? #f])
  (when (not first?)
    (with-time "recompiling"
      (managed-compile-zo trans.rkt)))
  (with-time "reloading"
    (parameterize ([current-namespace (make-base-namespace)])
      (attach!)
      (set! trans (dynamic-require trans.rkt 'transform@)))))

(define gadgets+unit (make-hash))

(define (refresh-menu add)
  (define ((load! gadget)) 
    (with-time "reloading gadgets"
      (parameterize ([current-namespace (make-base-namespace)])
        (attach!)
        (define tool
          (dynamic-require (build-path gadgets/ gadget) 'tool@))
        (hash-set! gadgets+unit gadget tool)
        (for ([v (in-weak-hash-values reload-observers)])
          (v gadget tool)))))
  
  (define ((unload! gadget))
    (hash-remove! gadgets+unit gadget)
    (for ([v (in-weak-hash-values reload-observers)])
      (v gadget)))
  
  (for ([gadget (in-list (directory-list gadgets/))]
        #:when (regexp-match? #rx"^tool.*\\.rkt$"
                              (path->string gadget)))
    (cond
      [(hash-ref gadgets+unit gadget (λ () #f))
       (add (format "reload ~a" gadget) (load! gadget))]
      [else
       (add (format "load ~a" gadget) (load! gadget))]))
  (for ([gadget (in-hash-keys gadgets+unit)])
    (add (format "unload ~a" gadget) (unload! gadget))))

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
          (for ([item (in-list (send me get-items))])
            (send item delete))
          (refresh-menu
           (λ (str cb)
             (new menu-item% [parent me]
                  [label str] [callback (λ (a b) (cb) (refresh-demand))]))))
        
        (define useless-menu
          (new menu% [parent menu-bar] [label "&Useless"]))

        (new menu-item% [parent useless-menu]
             [label "Reload transform"]
             [callback (λ (m e) (reload-transform))])

        (new menu-item% [parent useless-menu]
             [label "Refresh Gadgets"]
             [callback (λ (m e)
                         (refresh-demand))])

        (define me
          (new menu%
               [parent useless-menu]
               [label "Gadgets"]))

        (refresh-demand)))

    (define unit-instances (make-hash))

    (define unit-observers (make-weak-hash))

    (define unit-obserser
      (case-lambda
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

    (for ([(path tool) (in-hash gadgets+unit)])
      (unit-obserser path tool))
    
    (hash-set! reload-observers unit-instances unit-obserser)

    (define (compose-gadgets insts)
      (define surs (filter values (hash-values insts)))
      (foldr surrogate-compose (new surrogate%) surs))

    (define (observer instances key set-sur!)
      (case-lambda
        [(path)
         (hash-remove! instances path)
         (set-sur! (if (hash-empty? instances)
                       #f
                       (compose-gadgets instances)))]
        [(path gadgets)
         (cond
           [(hash-ref gadgets key (λ () #f))
            =>
            (λ (it)
              (hash-set! instances path (new (it surrogate%))))]
           [else
            (hash-remove! instances path)])
         (set-sur! (if (hash-empty? instances)
                       #f
                       (compose-gadgets instances)))]))

    (define (load-already key set-sur!)
      (define instances (make-hash))
      (unless (hash-empty? unit-instances)
        (for ([(path gadgets) (in-hash unit-instances)])
          ((observer instances key set-sur!) path gadgets)))
      instances)
    
    (drracket:get/extend:extend-unit-frame frame-mixin)

    (define ((text-mixin key) %) 
      (class (host-mixin %)
        (inherit set-private-surrogate)
        (super-new)
        (define instances
          (load-already key
                        (λ (sur)
                          (set-private-surrogate sur))))
        (hash-set! unit-observers this
                   (observer instances key
                             (λ (sur)
                               (set-private-surrogate sur))))))

    (drracket:get/extend:extend-definitions-text
     (text-mixin 'definition-mixin))

    (drracket:get/extend:extend-interactions-text
     (text-mixin 'interaction-mixin))

    (define trans-here trans)
    (define append-here #f)

    (keymap:add-to-right-button-menu
     (let ([orig (keymap:add-to-right-button-menu)])
       (λ (menu ed ev)
         
         (orig menu ed ev)

         (cond
           [(and (eq? trans trans-here)
                 append-here)
            (append-here menu ed ev)]
           [else
            (define-values/invoke-unit trans
              (import drracket:tool^)
              (export transform^))
            (set! trans-here trans)
            (set! append-here append-options)
            (append-options menu ed ev)])
         
         )))))
(reload-transform #t)
