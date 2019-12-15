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

(define-script let-and-print
  #:label "let-and-print"
  (λ (selection #:editor ed)
    (send ed introduce-let-ans
          (send ed get-start-position))
    #f))

(define-script transpose
  #:label "transpose"
  (λ (selection #:editor ed)
    (send ed transpose-sexp
          (send ed get-start-position))
    #f))

(define-script wrap-all-with-begin
  #:label "wrap all with begin"
  (λ (selection #:editor ed)
    (send ed begin-edit-sequence)
    (define s (send ed get-start-position))
    (let loop ([e s])
      (cond
        [(send ed get-forward-sexp e) => loop]
        [else (send ed insert ")" e)
              (send ed insert "(begin\n" s)
              (send ed tabify-selection s (+ e 8))]))
    (send ed end-edit-sequence)
    #f))

(define-script wrap-all-with-let
  #:label "wrap all with let"
  (λ (selection #:editor ed)
    (send ed begin-edit-sequence)
    (define s (send ed get-start-position))
    (let loop ([e s])
      (cond
        [(send ed get-forward-sexp e) => loop]
        [else (send ed insert ")" e)
              (send ed insert "(let ()\n" s)
              (send ed tabify-selection s (+ e 9))]))
    (send ed end-edit-sequence)
    #f))

(define-script wrap-with-begin
  #:label "wrap with begin"
  (λ (selection #:editor ed)
    (define s (send ed get-start-position))
    (define e (send ed get-forward-sexp s))
    (when e 
      (send ed begin-edit-sequence)
      (send ed insert ")" e)
      (send ed insert "(begin\n" s)
      (send ed tabify-selection s (+ e 8))
      (send ed end-edit-sequence))
    #f))

(define-script wrap-with-let
  #:label "wrap with let"
  (λ (selection #:editor ed)
    (define s (send ed get-start-position))
    (define e (send ed get-forward-sexp s))
    (when e 
      (send ed begin-edit-sequence)
      (send ed insert ")" e)
      (send ed insert "(let ()\n" s)
      (send ed tabify-selection s (+ e 9))
      (send ed end-edit-sequence))
    #f))