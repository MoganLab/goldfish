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

(define-library (liii tui terminal virtual state)
  (export <virtual-terminal-state> make-vt-state vt-state? vt-width
    vt-width-set! vt-height vt-height-set! vt-cursor-x vt-cursor-x-set!
    vt-cursor-y vt-cursor-y-set! vt-visible? vt-visible?-set! vt-lines
    vt-lines-set! vt-raw vt-raw-set! make-char-row make-screen-lines
    check-dimension scroll-up-one-line!
  ) ;export
  (import (liii base) (liii error))
  (begin

    (define-record-type <virtual-terminal-state>
      (make-vt-state width height cursor-x cursor-y visible? lines raw)
      vt-state?
      (width vt-width vt-width-set!)
      (height vt-height vt-height-set!)
      (cursor-x vt-cursor-x vt-cursor-x-set!)
      (cursor-y vt-cursor-y vt-cursor-y-set!)
      (visible? vt-visible? vt-visible?-set!)
      (lines vt-lines vt-lines-set!)
      (raw vt-raw vt-raw-set!)
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

  ) ;begin
) ;define-library
