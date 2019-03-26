#lang racket
(require racket/gui drracket/tool framework racket/runtime-path
         "and-body.rkt" "scope-guard.rkt")

(provide tool@)

(define-runtime-module-path trans.rkt "transform.rkt")

(define trans void)

(define (reload-transform)
  (parameterize ([current-namespace (make-base-namespace)])
    (set! trans (dynamic-require trans.rkt 'append-options))))

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

    (keymap:add-to-right-button-menu
     (let ([orig (keymap:add-to-right-button-menu)])
       (λ (menu ed ev)
         
         (orig menu ed ev)
         
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
                      (define neo-pos (send ed get-forward-sexp pos))
                      (when neo-pos
                        (send ed tabify-selection pos neo-pos))))]))

          (trans add str ed ev))
         
         (void)
         )))))

(reload-transform)