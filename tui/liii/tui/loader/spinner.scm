;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui loader spinner)
  (export make-spinner spinner? make-loader loader? spinner-frames
    spinner-frame-index spinner-current-frame spinner-text spinner-set-text!
    spinner-prefix spinner-set-prefix! spinner-suffix spinner-set-suffix!
    spinner-interval spinner-set-interval! spinner-visible? spinner-set-visible!
    spinner-show! spinner-hide! spinner-status spinner-set-status!
    spinner-success! spinner-fail! spinner-error! spinner-warn! spinner-stop!
    spinner-tick! spinner-reset!
  ) ;export
  (import (liii base)
    (liii error)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii tui loader presets)
    (liii tui loader state)
  ) ;import
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

    (define (spinner? obj)
      (or (%spinner-data? obj)
        (and (component? obj) (%spinner-data? (component-data obj)))
      ) ;or
    ) ;define

    (define loader? spinner?)

    (define (%spinner-render sp width)
      (if (not (%sp-visible sp))
        '()
        (if (<= width 0)
          '("")
          (let* ((status (%sp-status sp))
                 (frame-str (if (eq? status 'spinning)
                              (list-ref (%sp-frames sp) (%sp-frame-idx sp))
                              (or (spinner-status-icon status) "")
                            ) ;if
                 ) ;frame-str
                 (prefix (%sp-prefix sp))
                 (text (%sp-text sp))
                 (suffix (%sp-suffix sp))
                 (has-text? (or (> (string-length text) 0) (> (string-length suffix) 0)))
                 (raw-line
                   (cond
                    ((and (> (string-length frame-str) 0) has-text?)
                     (string-append prefix frame-str " " text suffix)
                    ) ;
                    ((> (string-length frame-str) 0) (string-append prefix frame-str))
                    (else (string-append prefix text suffix))
                   ) ;cond
                 ) ;raw-line
                ) ;
            (list (truncate-to-width raw-line width ""))
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    (define (make-spinner . args)
      (let ((initial-text #f) (opts args))
        (when (and (pair? args) (string? (car args)))
          (set! initial-text (car args))
          (set! opts (cdr args))
        ) ;when
        (let* ((frames-spec (%keyword-value :frames opts 'dots))
               (text-val (or (%keyword-value :text opts #f) initial-text ""))
               (prefix-val (%keyword-value :prefix opts ""))
               (suffix-val (%keyword-value :suffix opts ""))
               (interval-val (%keyword-value :interval opts 80))
               (visible-val (%keyword-value :visible opts #t))
               (status-val (%keyword-value :status opts 'spinning))
               (on-change-val (%keyword-value :on-change opts #f))
               (frames (spinner-frames-resolve frames-spec))
              ) ;
          (unless (string? text-val)
            (type-error "make-spinner: text must be a string" text-val)
          ) ;unless
          (unless (string? prefix-val)
            (type-error "make-spinner: prefix must be a string" prefix-val)
          ) ;unless
          (unless (string? suffix-val)
            (type-error "make-spinner: suffix must be a string" suffix-val)
          ) ;unless
          (unless (and (integer? interval-val) (>= interval-val 0))
            (type-error "make-spinner: interval must be a non-negative integer"
              interval-val
            ) ;type-error
          ) ;unless
          (unless (or (not on-change-val) (procedure? on-change-val))
            (type-error "make-spinner: on-change must be a procedure or #f" on-change-val)
          ) ;unless
          (spinner-status-icon status-val)
          (let* ((sp-data (%make-spinner-data frames
                            0
                            text-val
                            prefix-val
                            suffix-val
                            interval-val
                            (and visible-val #t)
                            status-val
                            on-change-val
                            #f
                          ) ;%make-spinner-data
                 ) ;sp-data
                 (comp (make-component
                         :render       (lambda (w) (%spinner-render sp-data w))
                         :handle-input (lambda (k) #f)
                         :invalidate
                         (lambda () (%notify-spinner-change! sp-data) #t)
                       ) ;make-component
                 ) ;comp
                ) ;
            (%sp-comp-set! sp-data comp)
            (component-data-set! comp sp-data)
            comp
          ) ;let*
        ) ;let*
      ) ;let
    ) ;define

    (define make-loader make-spinner)

  ) ;begin
) ;define-library
