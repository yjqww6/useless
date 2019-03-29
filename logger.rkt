#lang racket
(define-logger useless)


(define-syntax-rule (with-time name body ...)
  (let-values ([(_ t1 t2 t3)
                (time-apply (λ () body ...) '())])
    (log-useless-debug "~a done in ~a ms" name t1)))

(provide (all-defined-out))