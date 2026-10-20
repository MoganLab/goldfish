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

(define-library (liii tui main-screen buffer)
  (export <screen-buffer> make-screen-buffer screen-buffer? screen-buffer-lines
    screen-buffer-lines-set! screen-buffer-prev-lines
    screen-buffer-prev-lines-set! screen-buffer-dirty? screen-buffer-dirty-set!
    screen-buffer-invalidate! screen-buffer-commit! screen-buffer-clear!
  ) ;export
  (import (scheme base) (scheme write) (liii base) (liii error))
  (begin

    (define-record-type <screen-buffer>
      (%make-screen-buffer lines prev-lines dirty)
      screen-buffer?
      (lines %screen-buffer-lines %screen-buffer-lines-set!)
      (prev-lines %screen-buffer-prev-lines %screen-buffer-prev-lines-set!)
      (dirty %screen-buffer-dirty %screen-buffer-dirty-set!)
    ) ;define-record-type

    (define <screen-buffer> '<screen-buffer>)

    (define (%check-buffer buf caller)
      (unless (screen-buffer? buf)
        (type-error (string-append caller ": expected a <screen-buffer>, got " (object->string buf))
        ) ;type-error
      ) ;unless
    ) ;define

    (define (make-screen-buffer . args)
      (let ((init-lines (if (pair? args) (car args) '())))
        (unless (list? init-lines)
          (type-error "make-screen-buffer: expected a list of strings")
        ) ;unless
        (%make-screen-buffer init-lines '() #t)
      ) ;let
    ) ;define

    (define (screen-buffer-lines buf)
      (%check-buffer buf "screen-buffer-lines")
      (%screen-buffer-lines buf)
    ) ;define

    (define (screen-buffer-lines-set! buf lines)
      (%check-buffer buf "screen-buffer-lines-set!")
      (unless (list? lines)
        (type-error "screen-buffer-lines-set!: expected a list of strings")
      ) ;unless
      (%screen-buffer-lines-set! buf lines)
    ) ;define

    (define (screen-buffer-prev-lines buf)
      (%check-buffer buf "screen-buffer-prev-lines")
      (%screen-buffer-prev-lines buf)
    ) ;define

    (define (screen-buffer-prev-lines-set! buf lines)
      (%check-buffer buf "screen-buffer-prev-lines-set!")
      (unless (list? lines)
        (type-error "screen-buffer-prev-lines-set!: expected a list of strings")
      ) ;unless
      (%screen-buffer-prev-lines-set! buf lines)
    ) ;define

    (define (screen-buffer-dirty? buf)
      (%check-buffer buf "screen-buffer-dirty?")
      (%screen-buffer-dirty buf)
    ) ;define

    (define (screen-buffer-dirty-set! buf val)
      (%check-buffer buf "screen-buffer-dirty-set!")
      (%screen-buffer-dirty-set! buf (if val #t #f))
    ) ;define

    (define (screen-buffer-invalidate! buf)
      (%check-buffer buf "screen-buffer-invalidate!")
      (%screen-buffer-dirty-set! buf #t)
    ) ;define

    (define (screen-buffer-commit! buf new-lines)
      (%check-buffer buf "screen-buffer-commit!")
      (unless (list? new-lines)
        (type-error "screen-buffer-commit!: expected a list of strings")
      ) ;unless
      (%screen-buffer-prev-lines-set! buf (%screen-buffer-lines buf))
      (%screen-buffer-lines-set! buf new-lines)
      (%screen-buffer-dirty-set! buf #f)
    ) ;define

    (define (screen-buffer-clear! buf)
      (%check-buffer buf "screen-buffer-clear!")
      (%screen-buffer-prev-lines-set! buf (%screen-buffer-lines buf))
      (%screen-buffer-lines-set! buf '())
      (%screen-buffer-dirty-set! buf #t)
    ) ;define

  ) ;begin
) ;define-library
