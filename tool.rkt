#lang racket
(require racket/gui drracket/tool framework racket/runtime-path
         "transform-sig.rkt" compiler/cm
         (only-in
          (combine-in syntax/parse
                      racket/unit
                      racket/unit/lang/reader
                      racket/runtime-config)))

(provide tool@)

(define-runtime-module-path trans-sig.rkt "transform-sig.rkt")
(define-runtime-path trans.rkt "transform.rkt")

(define trans void)

(define-logger useless)

(define-syntax-rule (with-time name body ...)
  (let-values ([(_ t1 t2 t3)
                (time-apply (λ () body ...) '())])
    (log-useless-debug "~a done in ~a ms" name t1)))

(define-syntax-rule (first e)
  (call-with-values
   (λ () e)
   (λ (a . _) a)))

(define (reload-transform)
  (with-time "recompiling"
    (managed-compile-zo trans.rkt))
  (with-time "reloading"
    (parameterize ([current-namespace (make-base-namespace)])
      (define ns (variable-reference->namespace (#%variable-reference)))
      (namespace-attach-module ns 'racket)
      (namespace-attach-module ns 'racket/gui)
      (namespace-attach-module ns 'racket/unit)
      (namespace-attach-module ns 'racket/unit/lang/reader)
      (namespace-attach-module ns 'racket/runtime-config)
      (namespace-attach-module ns 'syntax/parse)
      (namespace-attach-module ns 'syntax/parse/define)
      (namespace-attach-module ns 'framework)
      (namespace-attach-module ns 'drracket/tool)
      (namespace-attach-module ns trans-sig.rkt)
      (set! trans (dynamic-require trans.rkt 'transform@)))))

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
             [callback (λ (m e) (reload-transform))])))
    
    (drracket:get/extend:extend-unit-frame frame-mixin)

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
(reload-transform)