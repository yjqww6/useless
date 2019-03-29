#lang racket

(require racket/gui drracket/tool framework)

(require "../methods.rkt" "gadget-sig.rkt" "../logger.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export gadget^)

  (define m
    (mixin (c:surrogate<%>) ()
      (define ts 0)
      (define times 0)
      (define/override (on-char ths super event)
        (cond
          [(memq (send event get-key-code) '(wheel-up wheel-down))
           (define es (send event get-time-stamp))
           (cond
             [(> (- es ts) 200)

              (define old-step (send ths wheel-step))
              (send ths wheel-step (* (+ times 1) old-step))
              (super event)
              (send ths wheel-step old-step)
              
              (set! ts es)
              (set! times 0)]
             [else
              (set! times (+ 1 times))])]
          [else
           (super event)]))
      (super-new)))

  (define gadgets
    (hasheq 'definition-canvas-mixin m 'interaction-canvas-mixin m)))