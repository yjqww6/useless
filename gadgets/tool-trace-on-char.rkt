#lang racket

(require racket/gui drracket/tool framework)

(require "../methods.rkt" "gadget-sig.rkt" "../logger.rkt" "module-transfer-sig.rkt")

(provide tool@)

(define-unit tool@
  (import drracket:tool^ module-transfer^)
  (export gadget^)
 
  (define interaction-mixin
    (mixin (surrogate<%>) ()
      (define/override (on-char ths super event)
        (log-useless-debug "on-char ~a" (send event get-key-code))
        (with-time "paths"
          (send ths drcomplete:path-completions))
        (super event))
      (super-new)))

  (define gadgets
    (hasheq 'interaction-mixin interaction-mixin)))