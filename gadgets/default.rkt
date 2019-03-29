#lang racket
(require racket/runtime-path)
(define defaults (map string->path '("tool-transform.rkt")))

(define-runtime-module-path ../methods.rkt "../methods.rkt")
(define-runtime-module-path gadget-sig.rkt "gadget-sig.rkt")
(define-runtime-module-path module-transfer-sig.rkt "module-transfer-sig.rkt")
(define-runtime-module-path ../logger.rkt "../logger.rkt")

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
                 gadget-sig.rkt
                 module-transfer-sig.rkt))

(provide defaults modules)