;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
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

(define-library (liii tui terminal virtual state)
  (export <virtual-terminal-state> make-vt-state vt-state? vt-width
    vt-width-set! vt-height vt-height-set! vt-cursor-x vt-cursor-x-set!
    vt-cursor-y vt-cursor-y-set! vt-visible? vt-visible?-set! vt-lines
    vt-lines-set! vt-raw vt-raw-set! vt-keys vt-keys-set! make-char-row
    make-screen-lines check-dimension scroll-up-one-line! advance-cursor-line!
    put-char! copy-row-chars!
  ) ;export
  (import (liii base) (liii error))
  (begin

    (define-record-type <virtual-terminal-state>
      (make-vt-state width height cursor-x cursor-y visible? lines raw keys)
      vt-state?
      (width vt-width vt-width-set!)
      (height vt-height vt-height-set!)
      (cursor-x vt-cursor-x vt-cursor-x-set!)
      (cursor-y vt-cursor-y vt-cursor-y-set!)
      (visible? vt-visible? vt-visible?-set!)
      (lines vt-lines vt-lines-set!)
      (raw vt-raw vt-raw-set!)
      (keys vt-keys vt-keys-set!)
    ) ;define-record-type

    (define <virtual-terminal-state> '<virtual-terminal-state>)

    (define (make-char-row width)
      (make-vector width #\space)
    ) ;define

    (define (make-screen-lines width height)
      (let ((v (make-vector height)))
        (let loop
          ((i 0))
          (if (< i height)
            (begin
              (vector-set! v i (make-char-row width))
              (loop (+ i 1))
            ) ;begin
            v
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (check-dimension n name)
      (unless (and (integer? n) (exact? n))
        (type-error (string-append name ": expected integer, got " (object->string n)))
      ) ;unless
      (when (<= n 0)
        (value-error (string-append name ": expected positive integer, got " (number->string n))
        ) ;value-error
      ) ;when
    ) ;define

    (define (scroll-up-one-line! state)
      (let* ((lines (vt-lines state)) (h (vt-height state)) (w (vt-width state)))
        (let loop
          ((i 0))
          (when (< i (- h 1))
            (vector-set! lines i (vector-ref lines (+ i 1)))
            (loop (+ i 1))
          ) ;when
        ) ;let
        (vector-set! lines (- h 1) (make-char-row w))
      ) ;let*
    ) ;define

    (define (advance-cursor-line! state)
      (vt-cursor-x-set! state 0)
      (let ((ny (+ (vt-cursor-y state) 1)))
        (if (>= ny (vt-height state))
          (begin
            (scroll-up-one-line! state)
            (vt-cursor-y-set! state (- (vt-height state) 1))
          ) ;begin
          (vt-cursor-y-set! state ny)
        ) ;if
      ) ;let
    ) ;define

    (define (put-char! state ch)
      (let ((cx (vt-cursor-x state))
            (cy (vt-cursor-y state))
            (w (vt-width state))
            (h (vt-height state))
           ) ;
        (when (and (< cx w) (< cy h))
          (let ((row (vector-ref (vt-lines state) cy)))
            (vector-set! row cx ch)
          ) ;let
        ) ;when
        (vt-cursor-x-set! state (+ cx 1))
      ) ;let
    ) ;define

    (define (copy-row-chars! old-row new-row copy-len)
      (let loop
        ((j 0))
        (when (< j copy-len)
          (vector-set! new-row j (vector-ref old-row j))
          (loop (+ j 1))
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
