#lang racket

(require syntax/parse (for-syntax syntax/parse))

(provide ~ relocate flat current->text gen cases)

(define (just str orig?)
  (if orig?
      str
      (string-append "(" str ")")))
  
(define (un-improper str orig?)
  (if orig?
      (string-append ". " str)
      str))

(define (maybe-improper s)
  (define str (string-trim s))
  (cond
    [(regexp-match #px"^[\\(\\[\\{](.*)[\\)\\]\\}]$" str) => second]
    [else (string-append ". " str)]))

(define (->text s proper? cvt)
  (define old (cvt s))
  (define orig? (or (syntax-original? s) (gen? s)))
  (match* (orig? proper?)
    [(#t #t) old]
    [(#t #f) (maybe-improper old)]
    [(#f #f) (un-improper old orig?)]
    [(#f #t) (just old orig?)]))

(define current->text
  (make-parameter
   (λ (s)
     (cond
       [(syntax-property s 'transform)
        (format "~a" (syntax->datum s))]))))

(define (fmt s proper?)
  (->text s proper? (current->text)))

(define (gen s)
  (syntax-property (datum->syntax #f s) 'transform #t))
  
(define (gen? s)
  (syntax-property s 'transform))

(define (loc stx)
  (cons (syntax-position stx)
        (syntax-span stx)))

(define (flat stx)
  (define l (loc stx))
  (let loop ([stx stx])
    (syntax-case stx ()
      [() '()]
      [(h . t)
       (let ([a (loc #'t)])
         (if (equal? a l) 
             (cons #'h (flat #'t)) 
             (list #'h #'t)))]
      [_ (list stx)])))

(define (relocate ls)
  (cond
    [(null? ls) (values 1 0)]
    [else
     (define-values (a b)
       (for/fold ([a +inf.0]
                  [b 0])
                 ([stx (in-list ls)])
         (define p (syntax-position stx))
         (define s (syntax-span stx))
         (values (min a p)
                 (max b (+ p s)))))
     (define ea (inexact->exact a))
     (values ea (- b ea))]))

(define (join lss sep)
  (if (null? lss)
      '()
      (append (car lss)
              (let loop ([lss (cdr lss)])
                (if (null? lss)
                    '()
                    (cons sep (append (car lss) (loop (cdr lss)))))))))

(define-syntax-rule (cases obj clauses ...)
  (let ([it obj])
    (syntax-parse it
      clauses
      [_ (void)])
    ...))

(define-syntax (~ stx)

  (define (get-sep l r)
    (if (< l r) #''("\n") #''()))

  (define (convert-list ls [line 1])
    (syntax-parse ls
      [()
       (values #''("") 0)]
      [((~datum unquote) a:expr)
       (values #'(list (fmt a #f)) (or (syntax-line #'a) line))]
      [((~and here ((~datum ...@) . h)) (~and me ((~datum ....) [x:id e:expr] ...)) . t)
       #:do [(define-values (hs hl) (convert-list #'h (syntax-line #'here)))
             (define-values (ts tl) (convert-list #'t hl))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (define ss
         (if (< hl (syntax-line #'me))
             #'(let ([l (filter
                         non-empty-string?
                         (join
                          (for/list ([x (in-list (syntax->list e))] ...)
                            ht)
                          '"\n"))])
                 (if (null? l)
                     tt
                     (cons
                      (string-join l)
                      (append sep tt))))
             #'(let ([l (filter
                         non-empty-string?
                         (apply append
                                (for/list ([x (in-list (syntax->list e))]
                                           ...)
                                  ht)))])
                 (if (null? l)
                     tt
                     (append l sep tt)))))
       (values ss hl)]
      [(h (~and me ((~datum ....) [x:id e:expr] ...)) . t)
       #:do [(define-values (hs hl) (convert #'h line))
             (define-values (ts tl) (convert-list #'t))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values
        (if (< hl (syntax-line #'me))
            #'(cons
               (string-join
                (filter
                 non-empty-string?
                 (for/list ([x (in-list (syntax->list e))] ...)
                   ht))
                "\n")
               (append sep tt))
            #'(append
               (for/list ([x (in-list (syntax->list e))] ...)
                 ht)
               sep
               tt))
        hl)]
      [(h . t)
       #:do [(define-values (hs hl) (convert #'h line))
             (define-values (ts tl) (convert-list #'t hl))]
       #:with ht hs
       #:with tt ts
       #:with sep (get-sep hl tl)
       (values #'(cons ht (append sep tt)) hl)]))

  (define (convert stx [line 1])
    (syntax-parse stx
      [((~datum unquote) a:expr)
       (values #'(fmt a #t) (or (syntax-line stx) line))]
      [((~or (~and (~datum syntax) (~bind [p #''"#'"]))
             (~and (~datum quote) (~bind [p #''"'"]))) form)
       #:do [(define-values (fs fl) (convert #'form line))]
       #:with f fs
       (values #'(string-append p f) (syntax-line stx))]
      [()
       (values #''"()" (syntax-line stx))]
      [(~and l (a . b))
       #:do [(define lshape (or (syntax-property #'l 'paren-shape) #\())
             (define rshape (case lshape
                              [(#\() #\)]
                              [(#\[) #\]]
                              [(#\{) #\}]))
             (define-values (t _) (convert-list #'(a . b) line))]
       #:with t t
       (values
        #`(string-append
           (~a #,lshape)
           (string-join (filter non-empty-string? t))
           (~a #,rshape))
        (syntax-line stx))]
      [thing (values #`'#,(format "~a" (syntax->datum stx))
                     (syntax-line stx))]))
  
  (syntax-case stx ()
    [(_ t)
     (let-values ([(t l) (convert #'t)])
       t)]))