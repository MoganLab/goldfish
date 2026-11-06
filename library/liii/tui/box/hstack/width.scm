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

(define-library (liii tui box hstack width)
  (export calculate-hstack-widths repeat-item)
  (import (liii base) (liii error) (liii list))
  (begin

    (define (repeat-item item n)
      (make-list n item)
    ) ;define

    (define (%adjust-last-width lst avail)
      (let* ((r (reverse lst)) (alloc-sum (apply + r)) (diff (- avail alloc-sum)))
        (append (reverse (cdr r)) (list (max 0 (+ (car r) diff))))
      ) ;let*
    ) ;define

    (define (%widths-auto avail num-cols)
      (let* ((base (quotient avail num-cols)) (rem (remainder avail num-cols)))
        (map
          (lambda (i) (+ base (if (< i rem) 1 0)))
          (iota num-cols)
        ) ;map
      ) ;let*
    ) ;define

    (define (%widths-ratios-or-weights avail sum-w widths-spec)
      (let ((denom (if (> sum-w 0) sum-w 1)))
        (%adjust-last-width
          (map
            (lambda (w) (max 0 (inexact->exact (round (/ (* w avail) denom)))))
            widths-spec
          ) ;map
          avail
        ) ;%adjust-last-width
      ) ;let
    ) ;define

    (define (%widths-fixed avail sum-w widths-spec)
      (if (> sum-w avail)
        (%widths-ratios-or-weights avail sum-w widths-spec)
        (let* ((rev-w (reverse widths-spec)) (last-w (+ (car rev-w) (- avail sum-w))))
          (reverse (cons last-w (cdr rev-w)))
        ) ;let*
      ) ;if
    ) ;define

    (define (%all-numbers? lst)
      (every number? lst)
    ) ;define

    (define (%has-ratios? lst)
      (every (lambda (x) (or (inexact? x) (<= x 1))) lst)
    ) ;define

    (define (%widths-mixed avail widths-spec)
      (let* ((fixed-sum 0) (auto-count 0))
        (for-each
          (lambda (w)
            (if (number? w)
              (set! fixed-sum (+ fixed-sum w))
              (set! auto-count (+ auto-count 1))
            ) ;if
          ) ;lambda
          widths-spec
        ) ;for-each
        (let* ((rem-for-auto (max 0 (- avail fixed-sum)))
               (auto-base (if (> auto-count 0) (quotient rem-for-auto auto-count) 0))
               (auto-rem (if (> auto-count 0) (remainder rem-for-auto auto-count) 0))
               (auto-idx 0)
              ) ;
          (map
            (lambda (w)
              (if (number? w)
                w
                (let ((res (+ auto-base (if (< auto-idx auto-rem) 1 0))))
                  (set! auto-idx (+ auto-idx 1))
                  res
                ) ;let
              ) ;if
            ) ;lambda
            widths-spec
          ) ;map
        ) ;let*
      ) ;let*
    ) ;define

    (define (%widths-by-numbers avail widths-spec)
      (let ((sum-w (apply + widths-spec)))
        (if (or (%has-ratios? widths-spec) (and (< sum-w 10) (>= avail 10)))
          (%widths-ratios-or-weights avail sum-w widths-spec)
          (%widths-fixed avail sum-w widths-spec)
        ) ;if
      ) ;let
    ) ;define

    (define (calculate-hstack-widths total-width num-cols spacing widths-spec)
      (if (<= num-cols 0)
        '()
        (let* ((total-spacing (* spacing (- num-cols 1)))
               (avail (max 0 (- total-width total-spacing)))
              ) ;
          (cond ((= num-cols 1) (list avail))
                ((or (eq? widths-spec 'auto) (not widths-spec)) (%widths-auto avail num-cols))
                ((pair? widths-spec)
                 (unless (= (length widths-spec) num-cols)
                   (value-error "make-hstack: length of widths does not match number of children")
                 ) ;unless
                 (if (%all-numbers? widths-spec)
                   (%widths-by-numbers avail widths-spec)
                   (%widths-mixed avail widths-spec)
                 ) ;if
                ) ;
                (else (value-error "make-hstack: invalid widths specification"))
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
