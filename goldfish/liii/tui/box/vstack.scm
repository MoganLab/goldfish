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

(define-library (liii tui box vstack)
  (export make-vstack vstack?)
  (import (liii base) (liii error) (liii tui component))
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
              (let loop
                ((rest args) (children-acc '()))
                (cond ((null? rest) (cons (reverse children-acc) '()))
                      ((keyword? (car rest)) (cons (reverse children-acc) rest))
                      (else (loop (cdr rest) (cons (car rest) children-acc)))
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (%validate-child child)
      (let ((c (if (string? child) (make-text child) child)))
        (unless (component? c)
          (type-error "make-vstack: child must be a component or string")
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

    (define (%append-spacing acc spacing)
      (let loop
        ((s 0) (cur acc))
        (if (< s spacing) (loop (+ s 1) (cons "" cur)) cur)
      ) ;let
    ) ;define

    (define (%append-lines acc lines)
      (let loop
        ((rem lines) (cur acc))
        (if (null? rem) cur (loop (cdr rem) (cons (car rem) cur)))
      ) ;let
    ) ;define

    (define (%vstack-render children spacing width)
      (if (null? children)
        '()
        (let loop
          ((rem children) (first? #t) (acc '()))
          (if (null? rem)
            (reverse acc)
            (let* ((child (car rem))
                   (lines (component-render child width))
                   (with-space (if first? acc (%append-spacing acc spacing)))
                   (next-acc (%append-lines with-space lines))
                  ) ;
              (loop (cdr rem) #f next-acc)
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%vstack-handle-input children key)
      (let loop
        ((rest children))
        (if (null? rest)
          #f
          (let ((res (component-handle-input (car rest) key)))
            (if res res (loop (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
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
        (let* ((children (map %validate-child raw-children))
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
