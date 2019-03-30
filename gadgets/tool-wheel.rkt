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
      (define up 0)
      (define left 0)
      (define/override (on-char ths super event)
        (log-useless-debug "on-char ~a" (send event get-key-code))
        (define (finish)
          (define es (current-milliseconds))
          (when (> (- es ts) 120)

            (define old-step (send ths wheel-step))
            
            (cond
              [(> up 0)
               (send ths wheel-step (* up old-step))
               (super (new key-event% [key-code 'wheel-up]))]
              [(< up 0)
               (send ths wheel-step (* (- up) old-step))
               (super (new key-event% [key-code 'wheel-down]))])
             
            (cond
              [(> left 0)
               (send ths wheel-step (* left old-step))
               (super (new key-event% [key-code 'wheel-left]))]
              [(< left 0)
               (send ths wheel-step (* (- left) old-step))
               (super (new key-event% [key-code 'wheel-right]))])
            (send ths wheel-step old-step)
              
            (set! ts es)
            (set! up 0)
            (set! left 0)))
        
        (when (> (- (current-milliseconds) ts) 500)
          (set! up 0)
          (set! left 0))
        
        (case (send event get-key-code)
          [(wheel-up) (set! up (+ up 1)) (finish)]
          [(wheel-down) (set! up (- up 1)) (finish)]
          [(wheel-left) (set! left (+ left 1)) (finish)]
          [(wheel-right) (set! left (- left 1)) (finish)]
          [else
           (set! up 0)
           (set! left 0)
           (super event)]))
      (super-new)))

  (define gadgets
    (hasheq 'definition-canvas-mixin m 'interaction-canvas-mixin m)))