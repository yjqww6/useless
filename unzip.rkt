#lang racket/gui
(require file/unzip mrlib/path-dialog useless/gui-helper
         useless/scope-guard)

(define ((transcode-entry-reader reader #:from from #:to [to ""]) name . rest)
  (with-scope-guard guard
    (define cvt (bytes-open-converter from to))
    (unless cvt
      (error 'transcode-entry-reader
             "no converter for ~a" from))
    (guard (bytes-close-converter cvt))
    (apply reader (let-values ([(b _1 _2)
                                (bytes-convert cvt name)])
                    b)
           rest)))

(define (put-dir)
  (define d (new path-dialog% [put? #t] [dir? #t] [filters #f]))
  (λ () (send d run)))

(define file-field%
  (class horizontal-panel%
    (init [label #f])
    (init-field [way get-file] [filter values])
    (super-new [stretchable-height #f])
    
    (define text (new text-field% [label label] [parent this]))
    
    (define btn (new button% [label "..."] [parent this]
                     [callback
                      (λ (b e)
                        (define f (cond [(way) => filter] [else #f]))
                        (when f
                          (send text set-value (path->string f))))]))
    
    (define/public (set-value str)
      (send text set-value str))
    
    (define/public (get-value)
      (send text get-value))

    (define/override (on-drop-file pathname)
      (define f (filter (path->string pathname)))
      (when f
        (send text set-value f)))
    
    (send this accept-drop-files #t)
    ))

(: f (frame% [label "unzip"] [width 320] [height 120])
   (: (menu-bar%)
      (: menu (menu% [label "edit"]))
      (append-editor-operation-menu-items menu #t))
   (: in (file-field% [label "zip"]
                      [way (λ () (get-file #f #f #f #f #f #f
                                           '(("zip" "*.zip") ("Any" "*.*"))))]
                      [filter (λ (path)
                                (define dir (path-replace-extension path ""))
                                (send out set-value (path->string dir))
                                path)]))
   (: out (file-field% [label "out"]
                       [way put-dir]))
   (: (horizontal-panel%)
      (: btn (button% [label "unzip"]
                      [callback
                       (λ (b e)
                         (define c (send coding get-value))
                         (define i (send in get-value))
                         (define o (send out get-value))
                         (send btn enable #f)
                         (define t
                           (thread
                            (λ ()
                              (with-handlers ([exn:fail?
                                               (λ (e)
                                                 (message-box "error"
                                                              (exn-message e)))])
                                (define reader
                                  (let ([r (make-filesystem-entry-reader #:dest o)])
                                    (if (string=? c "default")
                                        r
                                        (transcode-entry-reader r #:from c))))
                                (unzip i reader)))))
                         (yield t)
                         (send btn enable #t)
                         )]))
      (: coding (combo-field%
                 [label "coding"]
                 [choices '("default" "gbk" "shift-jis" "utf-8")]
                 [init-value "default"])))
   (send f show #t))