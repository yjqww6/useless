#lang racket
(require framework/private/mode "surrogate.rkt")

(define-local-member-name set-private-surrogate get-private-surrogate)

(define-values (host-mixin host<%> surrogate% surrogate<%> surrogate-compose)
  (let ()
    (define-syntax-rule (g m ...)
      (surrogate #:set-surrogate set-private-surrogate
                 #:get-surrogate get-private-surrogate

                 (override get-word-at (pos))
                 (override get-all-words ())
                 
                 m ...))
    (surrogate-methods g)))

(provide (all-defined-out))