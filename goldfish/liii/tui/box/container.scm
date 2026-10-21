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

(define-library (liii tui box container)
  (export make-container container? container-add! container-clear!
    container-children
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii tui component)
    (liii tui box vstack)
  ) ;import
  (begin

    (define (container? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'container))
        ) ;let
      ) ;and
    ) ;define

    (define (%check-container comp caller)
      (unless (container? comp)
        (type-error (string-append caller ": expected a container"))
      ) ;unless
    ) ;define

    ;; children 在 data 中倒序存储，container-add! 为 O(1)；读取时 reverse
    (define (container-children comp)
      (%check-container comp "container-children")
      (reverse (cadr (component-data comp)))
    ) ;define

    (define (container-add! comp child)
      (%check-container comp "container-add!")
      (let ((c (%validate-child child "container-add!")) (d (component-data comp)))
        (set-car! (cdr d) (cons c (cadr d)))
        comp
      ) ;let
    ) ;define

    (define (container-clear! comp)
      (%check-container comp "container-clear!")
      (set-car! (cdr (component-data comp)) '())
      comp
    ) ;define

    ;; container 即 children 可变、spacing 为 0 的 vstack，渲染/派发/失效直接复用
    (define (make-container)
      (let ((comp
              (make-component
                :render
                (lambda (self width) (%vstack-render (container-children self) 0 width))
                :handle-input
                (lambda (self key) (%vstack-handle-input (container-children self) key))
                :invalidate
                (lambda (self) (%vstack-invalidate (container-children self)))
              ) ;make-component
            ) ;comp
           ) ;
        (component-data-set! comp (list 'container '()))
        comp
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
