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

(define-library (liii tui box vstack)
  (export make-vstack vstack? %validate-child %vstack-render
    %vstack-handle-input %vstack-invalidate
  ) ;export
  (import (liii base) (liii error) (liii list) (liii tui component))
  (begin

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (vstack? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'vstack))
        ) ;let
      ) ;and
    ) ;define

    (define (%parse-children-and-opts args)
      (cond ((null? args) (cons '() '()))
            ((and (pair? (car args)) (not (component? (car args))))
             (cons (car args) (cdr args))
            ) ;
            ((null? (car args)) (cons '() (cdr args)))
            (else
              (cons
                (take-while (lambda (arg) (not (keyword? arg))) args)
                (drop-while (lambda (arg) (not (keyword? arg))) args)
              ) ;cons
            ) ;else
      ) ;cond
    ) ;define

    (define (%validate-child child caller)
      (let ((c (if (string? child) (make-text child) child)))
        (unless (component? c)
          (type-error (string-append caller ": child must be a component or string"))
        ) ;unless
        c
      ) ;let
    ) ;define

    (define (%validate-spacing spacing)
      (unless (and (integer? spacing) (>= spacing 0))
        (if (integer? spacing)
          (value-error "make-vstack: spacing must be non-negative")
          (type-error "make-vstack: spacing must be an integer")
        ) ;if
      ) ;unless
    ) ;define

    (define (%vstack-render children spacing width)
      (if (null? children)
        '()
        (let ((sep (make-list spacing ""))
              (rendered (map (lambda (child) (component-render child width)) children))
             ) ;
          (fold-right (lambda (lines acc) (if (null? acc) lines (append lines sep acc)))
            '()
            rendered
          ) ;fold-right
        ) ;let
      ) ;if
    ) ;define

    (define (%vstack-handle-input children key)
      (fold (lambda (child result) (or result (component-handle-input child key)))
        #f
        children
      ) ;fold
    ) ;define

    (define (%vstack-invalidate children)
      (for-each component-invalidate! children)
      #t
    ) ;define

    (define (make-vstack . args)
      (let* ((parsed (%parse-children-and-opts args))
             (raw-children (car parsed))
             (opts (cdr parsed))
             (spacing (%keyword-value :spacing opts 0))
            ) ;
        (%validate-spacing spacing)
        (let* ((children (map (lambda (ch) (%validate-child ch "make-vstack")) raw-children))
               (comp (make-component
                       :render
                       (lambda (self width) (%vstack-render children spacing width))
                       :handle-input
                       (lambda (self key) (%vstack-handle-input children key))
                       :invalidate (lambda (self) (%vstack-invalidate children))
                     ) ;make-component
               ) ;comp
              ) ;
          (component-data-set! comp (list 'vstack children))
          comp
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
