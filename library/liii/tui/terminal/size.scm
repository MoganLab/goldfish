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

(define-library (liii tui terminal size)
  (export terminal-detect-size)
  (import (scheme base)
    (scheme process-context)
    (liii base)
    (liii error)
    (liii os)
  ) ;import
  (begin

    (define (%parse-positive-int str default-val)
      (if (and (string? str) (not (string=? str "")))
        (let ((num (string->number str 10)))
          (if (and (integer? num) (exact? num) (> num 0)) num default-val)
        ) ;let
        default-val
      ) ;if
    ) ;define

    (define (terminal-detect-size . args)
      (let ((real-sz (os-terminal-size)))
        (if (and (pair? real-sz) (pair? (cdr real-sz)))
          real-sz
          (let* ((env-fn
                   (if (and (pair? args) (procedure? (car args)))
                     (car args)
                     get-environment-variable
                   ) ;if
                 ) ;env-fn
                 (col-str (env-fn "COLUMNS"))
                 (line-str (env-fn "LINES"))
                ) ;
            (list (%parse-positive-int col-str 80) (%parse-positive-int line-str 24))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
