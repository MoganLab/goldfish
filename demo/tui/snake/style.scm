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

(define-library (demo tui snake style)
  (export style-enabled? style-enable! style-disable! with-color-mode style-head
    style-body style-food style-title style-accent style-dim style-heartbeat
    style-game-over style-reset
  ) ;export
  (import (scheme base) (scheme process-context) (liii base) (liii error))
  (begin

    (define *style-active* #f)

    (define (%no-color-set?)
      (let ((nc (get-environment-variable "NO_COLOR")))
        (and (string? nc) (not (string=? nc "")))
      ) ;let
    ) ;define

    (define (style-enabled?)
      (and *style-active* (not (%no-color-set?)))
    ) ;define

    (define (style-enable!)
      (set! *style-active* #t)
      #t
    ) ;define

    (define (style-disable!)
      (set! *style-active* #f)
      #t
    ) ;define

    (define (with-color-mode enabled? thunk)
      (let ((old *style-active*))
        (dynamic-wind (lambda () (set! *style-active* (if enabled? #t #f)))
          thunk
          (lambda () (set! *style-active* old))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (%check-string s caller)
      (unless (string? s)
        (type-error (string-append caller ": expected string, got " (object->string s)))
      ) ;unless
    ) ;define

    (define (%apply-style prefix s caller)
      (%check-string s caller)
      (if (style-enabled?) (string-append prefix s "\x1b;[0m") s)
    ) ;define

    (define (style-head s)
      (%apply-style "\x1b;[1;32m" s "style-head")
    ) ;define

    (define (style-body s)
      (%apply-style "\x1b;[32m" s "style-body")
    ) ;define

    (define (style-food s)
      (%apply-style "\x1b;[1;31m" s "style-food")
    ) ;define

    (define (style-title s)
      (%apply-style "\x1b;[1;36m" s "style-title")
    ) ;define

    (define (style-accent s)
      (%apply-style "\x1b;[1;33m" s "style-accent")
    ) ;define

    (define (style-dim s)
      (%apply-style "\x1b;[90m" s "style-dim")
    ) ;define

    (define (style-heartbeat s)
      (%apply-style "\x1b;[1;35m" s "style-heartbeat")
    ) ;define

    (define (style-game-over s)
      (%apply-style "\x1b;[1;31m" s "style-game-over")
    ) ;define

    (define (style-reset)
      "\x1b;[0m"
    ) ;define

  ) ;begin
) ;define-library
