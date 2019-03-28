#lang racket

(require racket/gui drracket/tool framework)

(require "../methods.rkt" "gadget-sig.rkt" "../logger.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export gadget^)

  (define definition-mixin
    (mixin (surrogate<%>) ()
      (define/override (get-all-words ths super)
        (define-values (_ t1 t2 t3) (time-apply super '()))
        (log-useless-debug "get-all-words costs ~ams" t1)
        (apply values _))
      (super-new)))

  (define gadgets
    (hasheq 'definition-mixin definition-mixin)))