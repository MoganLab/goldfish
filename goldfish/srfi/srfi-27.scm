;; Copyright (C) Sebastian Egner (2002). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-library (srfi srfi-27)
  (import (goldfish) (scheme base) (liii error))
  (export random-integer random-real default-random-source make-random-source
          random-source? random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals)
  (begin
    (define-record-type <random-source>
      (%make-random-source handle)
      random-source?
      (handle random-source-handle))

    (define (make-random-source)
      (%make-random-source (g_random-source-create)))

    (define default-random-source (make-random-source))

    (define (check-source who source)
      (unless (random-source? source)
        (error 'wrong-type-arg who "expected random source" source)))

    (define (random-source-state-ref source)
      (check-source "random-source-state-ref" source)
      (g_random-source-state-ref (random-source-handle source)))

    (define (random-source-state-set! source state)
      (check-source "random-source-state-set!" source)
      (unless (and (list? state)
                   (= (length state) 3)
                   (eq? (car state) 'random-source-state)
                   (integer? (cadr state)) (exact? (cadr state))
                   (integer? (caddr state)) (exact? (caddr state))
                   (<= 0 (cadr state) (- (expt 2 64) 1))
                   (<= 0 (caddr state) (- (expt 2 64) 1))
                   (or (not (zero? (cadr state)))
                       (not (zero? (caddr state)))))
        (error 'wrong-type-arg "invalid random source state" state))
      (g_random-source-state-set! (random-source-handle source) state))

    (define (random-source-randomize! source)
      (check-source "random-source-randomize!" source)
      (g_random-source-randomize! (random-source-handle source)))

    (define (random-source-pseudo-randomize! source i j)
      (check-source "random-source-pseudo-randomize!" source)
      (unless (and (integer? i) (exact? i) (>= i 0))
        (error 'wrong-type-arg "expected non-negative exact integer" i))
      (unless (and (integer? j) (exact? j) (>= j 0))
        (error 'wrong-type-arg "expected non-negative exact integer" j))
      (g_random-source-pseudo-randomize! (random-source-handle source) i j))

    (define (random-source-make-integers source)
      (check-source "random-source-make-integers" source)
      (lambda (n)
        (unless (and (integer? n) (exact? n) (> n 0))
          (error 'wrong-type-arg "expected positive exact integer" n))
        (g_random-source-integer (random-source-handle source) n)))

    (define (random-source-make-reals source . unit-arg)
      (check-source "random-source-make-reals" source)
      (unless (<= (length unit-arg) 1)
        (error 'wrong-type-arg "expected at most one unit" unit-arg))
      (when (pair? unit-arg)
        (let ((unit (car unit-arg)))
          (unless (and (real? unit) (< 0 unit 1))
            (error 'wrong-type-arg "unit must be a real in (0,1)" unit))))
      (lambda ()
        (let ((r (g_random-source-real (random-source-handle source))))
          (if (null? unit-arg)
              r
              (let ((unit (car unit-arg)))
                (* (+ 1 (floor (* r (- (floor (/ 1 unit)) 1)))) unit))))))

    (define (random-integer n)
      ((random-source-make-integers default-random-source) n))

    (define (random-real)
      ((random-source-make-reals default-random-source)))))
