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

(define-library (liii tui loader state)
  (export %make-spinner-data %spinner-data? %sp-frames %sp-frames-set!
    %sp-frame-idx %sp-frame-idx-set! %sp-text %sp-text-set! %sp-prefix
    %sp-prefix-set! %sp-suffix %sp-suffix-set! %sp-interval %sp-interval-set!
    %sp-visible %sp-visible-set! %sp-status %sp-status-set! %sp-on-change
    %sp-on-change-set! %sp-comp %sp-comp-set! %unwrap-spinner
    %notify-spinner-change! spinner-frames spinner-frame-index
    spinner-current-frame spinner-text spinner-set-text! spinner-prefix
    spinner-set-prefix! spinner-suffix spinner-set-suffix! spinner-interval
    spinner-set-interval! spinner-visible? spinner-set-visible! spinner-show!
    spinner-hide! spinner-status spinner-set-status! spinner-success!
    spinner-fail! spinner-error! spinner-warn! spinner-stop! spinner-tick!
    spinner-reset!
  ) ;export
  (import (liii base)
    (liii error)
    (liii string)
    (liii tui component)
    (liii tui loader presets)
  ) ;import
  (begin

    (define-record-type <spinner-data>
      (%make-spinner-data frames frame-idx text prefix suffix interval visible
        status on-change comp
      ) ;%make-spinner-data
      %spinner-data?
      (frames %sp-frames %sp-frames-set!)
      (frame-idx %sp-frame-idx %sp-frame-idx-set!)
      (text %sp-text %sp-text-set!)
      (prefix %sp-prefix %sp-prefix-set!)
      (suffix %sp-suffix %sp-suffix-set!)
      (interval %sp-interval %sp-interval-set!)
      (visible %sp-visible %sp-visible-set!)
      (status %sp-status %sp-status-set!)
      (on-change %sp-on-change %sp-on-change-set!)
      (comp %sp-comp %sp-comp-set!)
    ) ;define-record-type

    (define (%unwrap-spinner obj caller)
      (cond ((%spinner-data? obj) obj)
            ((and (component? obj) (%spinner-data? (component-data obj)))
             (component-data obj)
            ) ;
            (else (type-error (string-append caller ": expected a spinner") obj))
      ) ;cond
    ) ;define

    (define (%notify-spinner-change! sp)
      (let ((on-change (%sp-on-change sp)) (comp (%sp-comp sp)))
        (when (procedure? on-change)
          (on-change (or comp sp))
        ) ;when
      ) ;let
    ) ;define

    (define (spinner-frames obj)
      (%sp-frames (%unwrap-spinner obj "spinner-frames"))
    ) ;define

    (define (spinner-frame-index obj)
      (%sp-frame-idx (%unwrap-spinner obj "spinner-frame-index"))
    ) ;define

    (define (spinner-current-frame obj)
      (let* ((sp (%unwrap-spinner obj "spinner-current-frame")) (status (%sp-status sp)))
        (if (eq? status 'spinning)
          (list-ref (%sp-frames sp) (%sp-frame-idx sp))
          (or (spinner-status-icon status) "")
        ) ;if
      ) ;let*
    ) ;define

    (define (spinner-text obj)
      (%sp-text (%unwrap-spinner obj "spinner-text"))
    ) ;define

    (define (spinner-set-text! obj text)
      (unless (string? text)
        (type-error "spinner-set-text!: text must be a string" text)
      ) ;unless
      (let ((sp (%unwrap-spinner obj "spinner-set-text!")))
        (%sp-text-set! sp text)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (spinner-prefix obj)
      (%sp-prefix (%unwrap-spinner obj "spinner-prefix"))
    ) ;define

    (define (spinner-set-prefix! obj prefix)
      (unless (string? prefix)
        (type-error "spinner-set-prefix!: prefix must be a string" prefix)
      ) ;unless
      (let ((sp (%unwrap-spinner obj "spinner-set-prefix!")))
        (%sp-prefix-set! sp prefix)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (spinner-suffix obj)
      (%sp-suffix (%unwrap-spinner obj "spinner-suffix"))
    ) ;define

    (define (spinner-set-suffix! obj suffix)
      (unless (string? suffix)
        (type-error "spinner-set-suffix!: suffix must be a string" suffix)
      ) ;unless
      (let ((sp (%unwrap-spinner obj "spinner-set-suffix!")))
        (%sp-suffix-set! sp suffix)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (spinner-interval obj)
      (%sp-interval (%unwrap-spinner obj "spinner-interval"))
    ) ;define

    (define (spinner-set-interval! obj interval)
      (unless (and (integer? interval) (>= interval 0))
        (type-error "spinner-set-interval!: interval must be a non-negative integer"
          interval
        ) ;type-error
      ) ;unless
      (let ((sp (%unwrap-spinner obj "spinner-set-interval!")))
        (%sp-interval-set! sp interval)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (spinner-visible? obj)
      (%sp-visible (%unwrap-spinner obj "spinner-visible?"))
    ) ;define

    (define (spinner-set-visible! obj visible)
      (let ((sp (%unwrap-spinner obj "spinner-set-visible!")))
        (%sp-visible-set! sp (and visible #t))
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (spinner-show! obj)
      (spinner-set-visible! obj #t)
    ) ;define

    (define (spinner-hide! obj)
      (spinner-set-visible! obj #f)
    ) ;define

    (define (spinner-status obj)
      (%sp-status (%unwrap-spinner obj "spinner-status"))
    ) ;define

    (define (spinner-set-status! obj status)
      (spinner-status-icon status)
      (let ((sp (%unwrap-spinner obj "spinner-set-status!")))
        (%sp-status-set! sp status)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (%set-terminal-status! obj status caller text-opt)
      (let ((sp (%unwrap-spinner obj caller)))
        (when (pair? text-opt)
          (let ((txt (car text-opt)))
            (unless (string? txt)
              (type-error (string-append caller ": text must be a string") txt)
            ) ;unless
            (%sp-text-set! sp txt)
          ) ;let
        ) ;when
        (%sp-status-set! sp status)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

    (define (spinner-success! obj . text-opt)
      (%set-terminal-status! obj 'success "spinner-success!" text-opt)
    ) ;define

    (define (spinner-fail! obj . text-opt)
      (%set-terminal-status! obj 'error "spinner-fail!" text-opt)
    ) ;define

    (define spinner-error! spinner-fail!)

    (define (spinner-warn! obj . text-opt)
      (%set-terminal-status! obj 'warning "spinner-warn!" text-opt)
    ) ;define

    (define (spinner-stop! obj)
      (%set-terminal-status! obj 'stopped "spinner-stop!" '())
    ) ;define

    (define (spinner-tick! obj)
      (let ((sp (%unwrap-spinner obj "spinner-tick!")))
        (when (eq? (%sp-status sp) 'spinning)
          (let* ((frames (%sp-frames sp))
                 (len (length frames))
                 (next-idx (modulo (+ (%sp-frame-idx sp) 1) len))
                ) ;
            (%sp-frame-idx-set! sp next-idx)
          ) ;let*
        ) ;when
        (%notify-spinner-change! sp)
        (spinner-current-frame sp)
      ) ;let
    ) ;define

    (define (spinner-reset! obj)
      (let ((sp (%unwrap-spinner obj "spinner-reset!")))
        (%sp-frame-idx-set! sp 0)
        (%sp-status-set! sp 'spinning)
        (%notify-spinner-change! sp)
        #t
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
