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

(define-library (liii tui main-screen diff)
  (export screen-diff screen-dirty-diff diff-op-type diff-op-index diff-op-text
    screen-diff-has-changes?
  ) ;export
  (import (scheme base) (scheme write) (liii base) (liii error) (liii list))
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
      (any
        (lambda (op) (not (eq? (car op) 'keep)))
        diff-ops
      ) ;any
    ) ;define

    (define (screen-diff old-lines new-lines)
      (%check-lines old-lines "screen-diff: old-lines")
      (%check-lines new-lines "screen-diff: new-lines")
      ;; 两个列表只顺序遍历一次，避免逐下标 list-ref 的平方复杂度。
      (let loop
        ((old-rest old-lines) (new-rest new-lines) (index 0) (ops '()))
        (cond
         ((and (pair? old-rest) (pair? new-rest))
          (let ((old-str (car old-rest)) (new-str (car new-rest)))
            (loop (cdr old-rest)
              (cdr new-rest)
              (+ index 1)
              (cons (if (string=? old-str new-str) (list 'keep index) (list 'update index new-str))
                ops
              ) ;cons
            ) ;loop
          ) ;let
         ) ;
         ((pair? new-rest)
          (loop old-rest
            (cdr new-rest)
            (+ index 1)
            (cons (list 'append index (car new-rest)) ops)
          ) ;loop
         ) ;
         ((pair? old-rest)
          (loop (cdr old-rest) new-rest (+ index 1) (cons (list 'clear index) ops))
         ) ;
         (else (reverse ops))
        ) ;cond
      ) ;let
    ) ;define

    (define (screen-dirty-diff old-lines new-lines)
      (%check-lines old-lines "screen-dirty-diff: old-lines")
      (%check-lines new-lines "screen-dirty-diff: new-lines")
      (let loop
        ((old-rest old-lines) (new-rest new-lines) (index 0) (ops '()))
        (cond
         ((pair? new-rest)
          (loop (if (pair? old-rest) (cdr old-rest) old-rest)
            (cdr new-rest)
            (+ index 1)
            (cons (list 'update index (car new-rest)) ops)
          ) ;loop
         ) ;
         ((pair? old-rest)
          (loop (cdr old-rest) new-rest (+ index 1) (cons (list 'clear index) ops))
         ) ;
         (else (reverse ops))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
