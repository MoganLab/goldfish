;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii tui box hstack width)
  (export calculate-hstack-widths repeat-item)
  (import (liii base) (liii error))
  (begin

    (define (repeat-item item n)
      (let loop
        ((i 0) (acc '()))
        (if (>= i n) acc (loop (+ i 1) (cons item acc)))
      ) ;let
    ) ;define

    (define (%adjust-last-width lst avail)
      (let* ((r (reverse lst)) (alloc-sum (apply + r)) (diff (- avail alloc-sum)))
        (append (reverse (cdr r)) (list (max 0 (+ (car r) diff))))
      ) ;let*
    ) ;define

    (define (%widths-auto avail num-cols)
      (let* ((base (quotient avail num-cols)) (rem (remainder avail num-cols)))
        (let loop
          ((i 0) (acc '()))
          (if (>= i num-cols)
            (reverse acc)
            (loop (+ i 1) (cons (+ base (if (< i rem) 1 0)) acc))
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%widths-ratios-or-weights avail sum-w widths-spec)
      (let loop
        ((cur widths-spec) (acc '()))
        (if (null? cur)
          (%adjust-last-width (reverse acc) avail)
          (let* ((w (car cur))
                 (denom (if (> sum-w 0) sum-w 1))
                 (col-w
                   (max 0 (inexact->exact (round (/ (* w avail) denom))))
                 ) ;col-w
                ) ;
            (loop (cdr cur) (cons col-w acc))
          ) ;let*
        ) ;if
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
      (let loop
        ((cur lst))
        (cond ((null? cur) #t)
              ((number? (car cur)) (loop (cdr cur)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%has-ratios? lst)
      (let loop
        ((cur lst))
        (cond ((null? cur) #t)
              ((or (inexact? (car cur)) (<= (car cur) 1)) (loop (cdr cur)))
              (else #f)
        ) ;cond
      ) ;let
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
