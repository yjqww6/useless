#lang racket

(require racket/gui drracket/tool framework)

(require "../methods.rkt" "gadget-sig.rkt" "../logger.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export gadget^)
  
  (define interaction-mixin
    (mixin (surrogate<%>) ()
      (define/override (on-char ths super event)
        (log-useless-debug "on-char ~a" (send event get-key-code))
        (super event))
      (super-new)))

  (define gadgets
    (hasheq 'interaction-mixin interaction-mixin)))