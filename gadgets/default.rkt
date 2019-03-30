#lang racket
(require racket/runtime-path)
(define defaults (map string->path
                      (append (if (eq? (system-type 'os) 'macosx)
                                  '("tool-wheel2.rkt")
                                  '())
                              '("tool-transform.rkt"))))

(define-runtime-path ../methods.rkt "../methods.rkt")
(define-runtime-path gadget-sig.rkt "gadget-sig.rkt")
(define-runtime-path ../logger.rkt "../logger.rkt")

(define modules (list 
                 'racket
                 'racket/gui
                 'racket/unit
                 'racket/runtime-config
                 'syntax/parse
                 'syntax/parse/define
                 'framework
                 'drracket/tool
                 ../methods.rkt
                 ../logger.rkt
                 gadget-sig.rkt))

(provide defaults modules)