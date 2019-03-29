#lang racket
(require racket/gui drracket/tool framework racket/runtime-path
         "transform-sig.rkt" compiler/cm "methods.rkt" "gadgets/gadget-sig.rkt"
         "logger.rkt"
         (only-in
          (combine-in syntax/parse racket/runtime-config)))

(provide tool@)

(define-runtime-path gadgets/ "gadgets")
(define-runtime-module-path methods.rkt "methods.rkt")
(define-runtime-module-path gadgets/gadget-sig.rkt "gadgets/gadget-sig.rkt")
(define-runtime-module-path logger.rkt "logger.rkt")

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
  (namespace-attach-module ns methods.rkt)
  (namespace-attach-module ns gadgets/gadget-sig.rkt)
  (namespace-attach-module ns logger.rkt))

(define gadgets+unit (make-hash))

(define (load! . gadgets) 
  (with-time "loading gadgets"
    (parameterize ([current-namespace (make-base-namespace)])
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
                    #:when (regexp-match? #rx"^tool.*\\.rkt$"
                                          (path->string gadget))
                    #:when (hash-ref gadgets+unit gadget (λ () #f)))
           gadget)))

(define (unload-all!)
  (apply unload!
         (for/list ([gadget (in-list (sort (hash-keys gadgets+unit) path<?))])
           gadget)))

(define (->label filename)
  (cond
    [(regexp-match #rx"^tool-(.*)\\.rkt$" (path->string filename))
     => second]
    [else filename]))

(define (refresh-menu add sep)
  (for ([gadget (in-list (sort (directory-list gadgets/) path<?))]
        #:when (regexp-match? #rx"^tool-.*\\.rkt$"
                              (path->string gadget)))
    (cond
      [(hash-ref gadgets+unit gadget (λ () #f))
       (add (format "reload ~a" (->label gadget)) (λ () (load! gadget)))]
      [else
       (add (format "load ~a" (->label gadget)) (λ () (load! gadget)))]))
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
                                    #:when (regexp-match? #rx"^tool.*\\.rkt$"
                                                          (path->string gadget)))
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

    (define (compose-gadgets insts)
      (define surs (map cdr (sort (hash->list insts) path<? #:key car)))
      (foldr surrogate-compose (car surs) (cdr surs)))

    (define (observer instances key set-sur!)
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
              (hash-set! instances path (new (it surrogate%))))]
           [else
            (hash-remove! instances path)])]))

    (define (load-already key set-sur!)
      (define instances (make-hash))
      (unless (hash-empty? unit-instances)
        (for ([(path gadgets) (in-hash unit-instances)])
          ((observer instances key set-sur!) path gadgets)))
      instances)
    
    (drracket:get/extend:extend-unit-frame frame-mixin)

    (define ((text-mixin key) %) 
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

(require "gadgets/default.rkt")
(apply load! defaults)
