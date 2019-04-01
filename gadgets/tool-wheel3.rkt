#lang racket

(require racket/gui drracket/tool framework)

(require "../methods.rkt" "gadget-sig.rkt" "../logger.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export gadget^)
  
  (define m
    (mixin (c:surrogate<%>) ()

      (define suspend? #f)
      
      (define/override (on-char this super event)
        (case (send event get-key-code)
          [(wheel-up wheel-down wheel-left wheel-right)
           (super event)
           (unless suspend?
             (send this suspend-flush)
             (set! suspend? #t)
             (new timer%
                  [notify-callback
                   (λ ()
                     (send this resume-flush)
                     (set! suspend? #f))]
                  [interval 120]
                  [just-once? #t]))]
          [else
           (super event)]))
      (super-new)))

  (define gadgets
    (hasheq 'definition-canvas-mixin m 'interaction-canvas-mixin m)))
