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

(define-library (liii tui clipboard)
  (export osc52-sequence osc52-copy-sequence clipboard-copy!
    native-clipboard-copy! native-clipboard-text native-clipboard-image-path
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base64)
    (liii tui terminal)
    (liii tui clipboard native)
  ) ;import
  (begin

    (define (osc52-sequence text)
      (string-append "\x1b;]52;c;" (string-base64-encode text) "\x07;")
    ) ;define

    (define osc52-copy-sequence osc52-sequence)

    (define (clipboard-copy! arg1 . rest)
      (let-values (((term text) (if (null? rest) (values #f arg1) (values arg1 (car rest)))))
        (let ((seq (osc52-sequence text)))
          (if term (begin (terminal-write term seq) (terminal-flush term)) (display seq))
          (native-clipboard-copy! text)
        ) ;let
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
