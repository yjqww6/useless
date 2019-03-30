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

      (define ths #f)
      (define by-thr #f)

      (define interval 0.12)

      (define/private (start)
        (with-handlers ([exn:break? void])
          (let loop ()
            (sleep (/ interval 2))
            (unless (and (= up 0) (= left 0))
                 
              (queue-callback
               (λ ()
                 (with-scope-guard guard
                   (set! by-thr #t)
                   (guard (set! by-thr #f))
                   (when ths
                     (with-method ([wheel-step (ths wheel-step)]
                                   [on-char (ths on-char)])
                       (define old-step (wheel-step))
            
                       (cond
                         [(> up 0)
                          (wheel-step (* up old-step))
                          (on-char (new key-event% [key-code 'wheel-up]))]
                         [(< up 0)
                          (wheel-step (* (- up) old-step))
                          (on-char (new key-event% [key-code 'wheel-down]))])
             
                       (cond
                         [(> left 0)
                          (wheel-step (* left old-step))
                          (on-char (new key-event% [key-code 'wheel-left]))]
                         [(< left 0)
                          (wheel-step (* (- left) old-step))
                          (on-char (new key-event% [key-code 'wheel-right]))])
                       (wheel-step old-step)
                    
                       (set! up 0)
                       (set! left 0))))))
                 
              (sleep (/ interval 2)))
            (loop))))
      
      (define thr #f)

      (define/override (on-enable-surrogate x)
        (set! thr 
              (thread
               (λ ()
                 (start)))))
      
      (define/override (on-disable-surrogate x)
        (when thr
          (break-thread thr)))

      (define/override (on-superwindow-show this super shown?)
        (cond
          [shown? (unless thr
                    (set! thr (thread (λ () (start)))))]
          [else (break-thread thr)
                (set! thr #f)])
        (super shown?))
      
      (define/override (on-char this super event)
        ;(log-useless-debug "on-char ~a" (send event get-key-code))
        (unless ths
          (set! ths this))
        (if by-thr
            (super event)
            (case (send event get-key-code)
              [(wheel-up) (set! up (+ up 1))]
              [(wheel-down) (set! up (- up 1))]
              [(wheel-left) (set! left (+ left 1))]
              [(wheel-right) (set! left (- left 1))]
              [else
               (set! up 0)
               (set! left 0)
               (super event)])))
      (super-new)))

  (define gadgets
    (hasheq 'definition-canvas-mixin m 'interaction-canvas-mixin m)))