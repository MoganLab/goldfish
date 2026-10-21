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

(define-library (liii tui box container)
  (export make-container container? container-add! container-clear!
    container-children
  ) ;export
  (import (scheme base) (liii base) (liii error) (liii tui component))
  (begin

    (define (%container-data comp)
      (let ((d (component-data comp)))
        (if (and (pair? d) (eq? (car d) 'container)) d #f)
      ) ;let
    ) ;define

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

    (define (%validate-child child caller)
      (let ((c (if (string? child) (make-text child) child)))
        (unless (component? c)
          (type-error (string-append caller ": child must be a component or string"))
        ) ;unless
        c
      ) ;let
    ) ;define

    (define (container-children comp)
      (%check-container comp "container-children")
      (let ((d (%container-data comp)))
        (cadr d)
      ) ;let
    ) ;define

    (define (container-add! comp child)
      (%check-container comp "container-add!")
      (let* ((c (%validate-child child "container-add!"))
             (d (%container-data comp))
             (cur (cadr d))
            ) ;
        (set-car! (cdr d) (append cur (list c)))
        comp
      ) ;let*
    ) ;define

    (define (container-clear! comp)
      (%check-container comp "container-clear!")
      (let ((d (%container-data comp)))
        (set-car! (cdr d) '())
        comp
      ) ;let
    ) ;define

    (define (%container-render comp width)
      (let ((children (container-children comp)))
        (let loop
          ((rem children) (acc '()))
          (if (null? rem)
            (apply append (reverse acc))
            (let* ((child (car rem)) (lines (component-render child width)))
              (loop (cdr rem) (cons lines acc))
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%container-handle-input comp key)
      (let ((children (container-children comp)))
        (let loop
          ((rest children))
          (if (null? rest)
            #f
            (let ((res (component-handle-input (car rest) key)))
              (if res res (loop (cdr rest)))
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%container-invalidate comp)
      (let ((children (container-children comp)))
        (for-each component-invalidate! children)
        #t
      ) ;let
    ) ;define

    (define (make-container)
      (let* ((d (list 'container '()))
             (comp (make-component
                     :render
                     (lambda (self width) (%container-render self width))
                     :handle-input
                     (lambda (self key) (%container-handle-input self key))
                     :invalidate (lambda (self) (%container-invalidate self))
                   ) ;make-component
             ) ;comp
            ) ;
        (component-data-set! comp d)
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
