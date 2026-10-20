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

(define-library (liii tui main-screen diff)
  (export screen-diff screen-dirty-diff diff-op-type diff-op-index diff-op-text
    screen-diff-has-changes?
  ) ;export
  (import (scheme base) (scheme write) (liii base) (liii error))
  (begin

    (define (%check-lines lst caller)
      (unless (list? lst)
        (type-error (string-append caller ": expected a list of strings, got " (object->string lst))
        ) ;type-error
      ) ;unless
      (for-each
        (lambda (x)
          (unless (string? x)
            (type-error (string-append caller ": expected string element, got " (object->string x))
            ) ;type-error
          ) ;unless
        ) ;lambda
        lst
      ) ;for-each
    ) ;define

    (define (diff-op-type op)
      (car op)
    ) ;define

    (define (diff-op-index op)
      (cadr op)
    ) ;define

    (define (diff-op-text op)
      (if (>= (length op) 3) (caddr op) #f)
    ) ;define

    (define (screen-diff-has-changes? diff-ops)
      (let loop
        ((ops diff-ops))
        (cond ((null? ops) #f)
              ((eq? (car (car ops)) 'keep) (loop (cdr ops)))
              (else #t)
        ) ;cond
      ) ;let
    ) ;define

    (define (screen-diff old-lines new-lines)
      (%check-lines old-lines "screen-diff: old-lines")
      (%check-lines new-lines "screen-diff: new-lines")
      (let* ((len-old (length old-lines))
             (len-new (length new-lines))
             (max-len (max len-old len-new))
            ) ;
        (let loop
          ((i 0) (acc '()))
          (if (= i max-len)
            (reverse acc)
            (let ((op
                    (cond
                     ((and (< i len-old) (< i len-new))
                      (let ((old-str (list-ref old-lines i)) (new-str (list-ref new-lines i)))
                        (if (string=? old-str new-str) (list 'keep i) (list 'update i new-str))
                      ) ;let
                     ) ;
                     ((and (>= i len-old) (< i len-new)) (list 'append i (list-ref new-lines i)))
                     (else (list 'clear i))
                    ) ;cond
                  ) ;op
                 ) ;
              (loop (+ i 1) (cons op acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (screen-dirty-diff old-lines new-lines)
      (%check-lines old-lines "screen-dirty-diff: old-lines")
      (%check-lines new-lines "screen-dirty-diff: new-lines")
      (let* ((len-old (length old-lines))
             (len-new (length new-lines))
             (max-len (max len-old len-new))
            ) ;
        (let loop
          ((i 0) (acc '()))
          (if (= i max-len)
            (reverse acc)
            (let ((op (if (< i len-new) (list 'update i (list-ref new-lines i)) (list 'clear i)))
                 ) ;
              (loop (+ i 1) (cons op acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
