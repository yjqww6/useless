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
  (when first?
    (with-time "recompiling"
      (managed-compile-zo trans.rkt)))
  (with-time "reloading"
    (parameterize ([current-namespace (make-base-namespace)])
      (attach!)
      (set! trans (dynamic-require trans.rkt 'transform@)))))

(define (reload-gadgets)
  (with-time "reloading gadgets"
    (parameterize ([current-namespace (make-base-namespace)])
      (attach!)
      (define gadgets
        (for/list ([gadget (in-list (directory-list gadgets/))]
                   #:when (regexp-match? #rx"^tool.*\\.rkt$" (path->string gadget)))
          (dynamic-require (build-path gadgets/ gadget) 'tool@)))
      (for ([v (in-weak-hash-values reload-observers)])
        (v gadgets)))))

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
        (define useless-menu
          (new menu% [parent menu-bar] [label "&Useless"]))

        (new menu-item% [parent useless-menu]
             [label "Reload transform"]
             [callback (λ (m e) (reload-transform))])
        (new menu-item% [parent useless-menu]
             [label "Reload gadgets"]
             [callback (λ (m e) (reload-gadgets))])
        (new menu-item% [parent useless-menu]
             [label "Unload gadgets"]
             [callback (λ (m e)
                         (for ([v (in-weak-hash-values reload-observers)])
                           (v '())))])))

    (define (compose-gadgets gadgets key)
      (new
       ((foldr compose values
               (filter
                values
                (for/list ([gadget (in-list gadgets)])
                  (define-values/invoke-unit gadget
                    (import drracket:tool^)
                    (export gadget^))
                  (hash-ref gadgets key (λ () #f)))))
        surrogate%)))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)

    (drracket:get/extend:extend-definitions-text
     (λ (%)
       (class (host-mixin %)
         (inherit set-private-surrogate)
         (super-new)
         (hash-set! reload-observers this
                    (λ (gadgets)
                      (set-private-surrogate
                       (compose-gadgets gadgets 'definition-mixin)))))))

    (drracket:get/extend:extend-interactions-text
     (λ (%)
       (class (host-mixin %)
         (inherit set-private-surrogate)
         (super-new)
         (hash-set! reload-observers this
                    (λ (gadgets)
                      (set-private-surrogate
                       (compose-gadgets gadgets 'interaction-mixin)))))))

    (define trans-here trans)
    (define append-here void)

    (keymap:add-to-right-button-menu
     (let ([orig (keymap:add-to-right-button-menu)])
       (λ (menu ed ev)
         
         (orig menu ed ev)

         (cond
           [(eq? trans trans-here) (append-here menu ed ev)]
           [else
            (define-values/invoke-unit trans
              (import drracket:tool^)
              (export transform^))
            (set! trans-here trans)
            (set! append-here append-options)
            (append-options menu ed ev)])
         
         )))))
(reload-transform #t)
(reload-gadgets)
