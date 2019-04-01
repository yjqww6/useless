#lang racket

(require racket/gui drracket/tool framework)

(require "../methods.rkt" "gadget-sig.rkt" "../logger.rkt" "../scope-guard.rkt" "../and-body.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export gadget^)
  
  (define m
    (mixin (c:surrogate<%>) ()
      (define up 0)
      (define left 0)
      
      (define interval 32)

      (define running? #f)
      
      (define timer #f)
      
      (define/override (on-char this super event)
        (define (start-timer)
          (unless timer
            (let loop ([int 16])
              (set!
               timer
               (new timer% [interval int] [just-once? #t]
                    [notify-callback
                     (λ ()
                       (cond
                         [(and (= up 0) (= left 0)) (set! timer #f)]
                         [else
                          (with-method ([wheel-step (this wheel-step)])
                            (define old-step (wheel-step))
            
                            (cond
                              [(> up 0)
                               (wheel-step (* up old-step))
                               (super (new key-event% [key-code 'wheel-up]))]
                              [(< up 0)
                               (wheel-step (* (- up) old-step))
                               (super (new key-event% [key-code 'wheel-down]))])
             
                            (cond
                              [(> left 0)
                               (wheel-step (* left old-step))
                               (super (new key-event% [key-code 'wheel-left]))]
                              [(< left 0)
                               (wheel-step (* (- left) old-step))
                               (super (new key-event% [key-code 'wheel-right]))])
                            (wheel-step old-step)
                            (set! up 0)
                            (set! left 0)
                            (loop interval))]))])))))
        
        (case (send event get-key-code)
          [(wheel-up) (set! up (+ up 1))
                      (start-timer)]
          [(wheel-down) (set! up (- up 1))
                        (start-timer)]
          [(wheel-left) (set! left (+ left 1))
                        (start-timer)]
          [(wheel-right) (set! left (- left 1))
                         (start-timer)]
          [else
           (when timer
             (send timer stop)
             (set! timer #f))
           (set! up 0)
           (set! left 0)
           (super event)]))
      (super-new)))

  (define gadgets
    (hasheq 'definition-canvas-mixin m 'interaction-canvas-mixin m)))
