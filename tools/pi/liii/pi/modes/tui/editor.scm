;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui editor)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii tui component)
    (liii tui editor)
    (liii tui editor buffer)
    (liii pi modes tui editor types)
    (liii pi modes tui editor handle)
    (liii pi modes tui editor render)
  ) ;import
  (export <pi-tui-editor-data> make-pi-tui-editor pi-tui-editor?
    pi-tui-editor-text pi-tui-editor-set-text! pi-tui-editor-clear!
    pi-tui-editor-lines pi-tui-editor-cursor pi-tui-editor-set-cursor!
    pi-tui-editor-prompt pi-tui-editor-set-prompt!
    pi-tui-editor-continuation-prompt pi-tui-editor-set-continuation-prompt!
    pi-tui-editor-placeholder pi-tui-editor-set-placeholder!
    pi-tui-editor-focused? pi-tui-editor-set-focused! pi-tui-editor-history
    pi-tui-editor-push-history! pi-tui-editor-handle-key pi-tui-editor-render
    pi-tui-editor-render-boxed pi-tui-editor-on-submit
    pi-tui-editor-set-on-submit! pi-tui-editor-on-change
    pi-tui-editor-set-on-change!
  ) ;export
  (begin

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-tui-editor? obj)
      (and (component? obj) (pi-tui-editor-data? (component-data obj)))
    ) ;define

    (define (%validate-editor ed caller)
      (unless (pi-tui-editor? ed)
        (type-error (string-append caller ": expected <pi-tui-editor>"))
      ) ;unless
    ) ;define

    (define (make-pi-tui-editor . opts)
      (let* ((initial-text (%keyword-value :text opts ""))
             (prompt (%keyword-value :prompt opts "> "))
             (cont-prompt (%keyword-value :continuation-prompt opts "  "))
             (ph (%keyword-value :placeholder opts "Ask a question or type / for commands...")
             ) ;ph
             (focused? (%keyword-value :focused? opts #t))
             (cursor-style (%keyword-value :cursor-style opts 'inverse))
             (on-sub (%keyword-value :on-submit opts #f))
             (on-ch (%keyword-value :on-change opts #f))
             (border (%keyword-value :border-style opts 'none))
             (hist-size (%keyword-value :history-size opts 100))
             (buf (make-buffer initial-text #t))
             (hist (make-history hist-size))
             (data (%make-pi-tui-editor-data buf hist prompt cont-prompt ph
                     focused? cursor-style on-sub on-ch border
                   ) ;%make-pi-tui-editor-data
             ) ;data
             (comp (make-component
                     :render
                     (lambda (self width) (%pi-tui-editor-render self width))
                     :handle-input
                     (lambda (self key) (%pi-tui-editor-handle-key self key))
                     :invalidate (lambda (self) #t)
                   ) ;make-component
             ) ;comp
            ) ;
        (component-data-set! comp data)
        comp
      ) ;let*
    ) ;define

    (define (pi-tui-editor-text ed)
      (%validate-editor ed "pi-tui-editor-text")
      (buffer-text (ed-data-buffer (component-data ed)))
    ) ;define

    (define (pi-tui-editor-set-text! ed text)
      (%validate-editor ed "pi-tui-editor-set-text!")
      (let* ((data (component-data ed)) (buf (ed-data-buffer data)))
        (buffer-set-text! buf text)
        (when (procedure? (ed-data-on-change data))
          ((ed-data-on-change data) ed text)
        ) ;when
      ) ;let*
    ) ;define

    (define (pi-tui-editor-clear! ed)
      (%validate-editor ed "pi-tui-editor-clear!")
      (let* ((data (component-data ed)) (buf (ed-data-buffer data)))
        (buffer-set-text! buf "")
        (when (procedure? (ed-data-on-change data))
          ((ed-data-on-change data) ed "")
        ) ;when
      ) ;let*
    ) ;define

    (define (pi-tui-editor-lines ed)
      (%validate-editor ed "pi-tui-editor-lines")
      (buffer-lines (ed-data-buffer (component-data ed)))
    ) ;define

    (define (pi-tui-editor-cursor ed)
      (%validate-editor ed "pi-tui-editor-cursor")
      (let ((buf (ed-data-buffer (component-data ed))))
        (list (buffer-row buf) (buffer-col buf))
      ) ;let
    ) ;define

    (define (pi-tui-editor-set-cursor! ed row col)
      (%validate-editor ed "pi-tui-editor-set-cursor!")
      (let* ((buf (ed-data-buffer (component-data ed)))
             (lines (buffer-lines buf))
             (total (length lines))
             (safe-row (max 0 (min row (- total 1))))
             (line (list-ref lines safe-row))
             (safe-col (max 0 (min col (string-length line))))
            ) ;
        (buffer-row-set! buf safe-row)
        (buffer-col-set! buf safe-col)
      ) ;let*
    ) ;define

    (define (pi-tui-editor-prompt ed)
      (%validate-editor ed "pi-tui-editor-prompt")
      (ed-data-prompt (component-data ed))
    ) ;define

    (define (pi-tui-editor-set-prompt! ed p)
      (%validate-editor ed "pi-tui-editor-set-prompt!")
      (ed-data-prompt-set! (component-data ed) p)
    ) ;define

    (define (pi-tui-editor-continuation-prompt ed)
      (%validate-editor ed "pi-tui-editor-continuation-prompt")
      (ed-data-continuation-prompt (component-data ed))
    ) ;define

    (define (pi-tui-editor-set-continuation-prompt! ed p)
      (%validate-editor ed "pi-tui-editor-set-continuation-prompt!")
      (ed-data-continuation-prompt-set! (component-data ed) p)
    ) ;define

    (define (pi-tui-editor-placeholder ed)
      (%validate-editor ed "pi-tui-editor-placeholder")
      (ed-data-placeholder (component-data ed))
    ) ;define

    (define (pi-tui-editor-set-placeholder! ed ph)
      (%validate-editor ed "pi-tui-editor-set-placeholder!")
      (ed-data-placeholder-set! (component-data ed) ph)
    ) ;define

    (define (pi-tui-editor-focused? ed)
      (%validate-editor ed "pi-tui-editor-focused?")
      (ed-data-focused? (component-data ed))
    ) ;define

    (define (pi-tui-editor-set-focused! ed f?)
      (%validate-editor ed "pi-tui-editor-set-focused!")
      (ed-data-focused?-set! (component-data ed) f?)
    ) ;define

    (define (pi-tui-editor-history ed)
      (%validate-editor ed "pi-tui-editor-history")
      (ed-data-history (component-data ed))
    ) ;define

    (define (pi-tui-editor-push-history! ed txt)
      (%validate-editor ed "pi-tui-editor-push-history!")
      (history-push! (ed-data-history (component-data ed)) txt)
    ) ;define

    (define (pi-tui-editor-handle-key ed key)
      (%validate-editor ed "pi-tui-editor-handle-key")
      (%pi-tui-editor-handle-key ed key)
    ) ;define

    (define (pi-tui-editor-render ed width)
      (%validate-editor ed "pi-tui-editor-render")
      (%pi-tui-editor-render ed width)
    ) ;define

    (define (pi-tui-editor-on-submit ed)
      (%validate-editor ed "pi-tui-editor-on-submit")
      (ed-data-on-submit (component-data ed))
    ) ;define

    (define (pi-tui-editor-set-on-submit! ed proc)
      (%validate-editor ed "pi-tui-editor-set-on-submit!")
      (ed-data-on-submit-set! (component-data ed) proc)
    ) ;define

    (define (pi-tui-editor-on-change ed)
      (%validate-editor ed "pi-tui-editor-on-change")
      (ed-data-on-change (component-data ed))
    ) ;define

    (define (pi-tui-editor-set-on-change! ed proc)
      (%validate-editor ed "pi-tui-editor-set-on-change!")
      (ed-data-on-change-set! (component-data ed) proc)
    ) ;define

  ) ;begin
) ;define-library
