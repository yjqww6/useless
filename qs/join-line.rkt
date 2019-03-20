#lang racket/base

(require quickscript racket/class "../and-body.rkt")

(define-script join-line
  #:label "join-line"
  #:shortcut #\j
  #:shortcut-prefix (ctl shift)
  (λ (selection #:editor ed)
    (define line (send ed position-line (send ed get-start-position)))
    (define line-end (send ed line-end-position line))
    (and-body
     (define f (send ed get-forward-sexp line-end))
     (define b (send ed get-backward-sexp f))
     (send ed delete line-end b))
    #f))
