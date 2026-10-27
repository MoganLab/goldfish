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

(define-library (liii tui box hstack)
  (export make-hstack hstack?)
  (import (liii base)
    (liii error)
    (liii list)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii tui box hstack width)
  ) ;import
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

    (define (hstack? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'hstack))
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

    (define (%validate-hstack-child child)
      (let ((c (if (string? child) (make-text child) child)))
        (unless (component? c)
          (type-error "make-hstack: child must be a component or string")
        ) ;unless
        c
      ) ;let
    ) ;define

    (define (%validate-hstack-spacing spacing)
      (unless (and (integer? spacing) (>= spacing 0))
        (if (integer? spacing)
          (value-error "make-hstack: spacing must be non-negative")
          (type-error "make-hstack: spacing must be an integer")
        ) ;if
      ) ;unless
    ) ;define

    (define (%pad-col-lines col-lines max-h valign)
      (let ((diff (- max-h (length col-lines))))
        (if (<= diff 0)
          col-lines
          (case valign
           ((top) (append col-lines (repeat-item "" diff)))
           ((bottom) (append (repeat-item "" diff) col-lines))
           ((center)
            (let ((t-diff (quotient diff 2)))
              (append (repeat-item "" t-diff) col-lines (repeat-item "" (- diff t-diff)))
            ) ;let
           ) ;
          ) ;case
        ) ;if
      ) ;let
    ) ;define

    (define (%fit-cell cell col-w)
      (let ((vis-w (display-width cell)))
        (cond ((> vis-w col-w) (truncate-to-width cell col-w ""))
              ((< vis-w col-w) (string-append cell (make-string (- col-w vis-w) #\space)))
              (else cell)
        ) ;cond
      ) ;let
    ) ;define

    (define (%render-hstack-row padded-cols col-widths space-str r)
      (string-join
        (map (lambda (col col-w) (%fit-cell (list-ref col r) col-w))
          padded-cols
          col-widths
        ) ;map
        space-str
      ) ;string-join
    ) ;define

    (define (%hstack-render children spacing widths-spec valign width)
      (let ((num-cols (length children)))
        (if (= num-cols 0)
          '()
          (let* ((col-widths (calculate-hstack-widths width num-cols spacing widths-spec))
                 (all-cols-lines (map (lambda (c w) (component-render c w)) children col-widths))
                 (max-h (apply max 0 (map length all-cols-lines)))
                ) ;
            (if (= max-h 0)
              '()
              (let* ((padded-cols (map (lambda (lines) (%pad-col-lines lines max-h valign)) all-cols-lines)
                     ) ;padded-cols
                     (space-str (make-string spacing #\space))
                    ) ;
                (map (lambda (r) (%render-hstack-row padded-cols col-widths space-str r))
                  (iota max-h)
                ) ;map
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%hstack-handle-input children key)
      (fold (lambda (child result) (or result (component-handle-input child key)))
        #f
        children
      ) ;fold
    ) ;define

    (define (%hstack-invalidate children)
      (for-each component-invalidate! children)
      #t
    ) ;define

    (define (make-hstack . args)
      (let* ((parsed (%parse-children-and-opts args))
             (raw-children (car parsed))
             (opts (cdr parsed))
             (spacing (%keyword-value :spacing opts 0))
             (widths-spec (%keyword-value :widths opts 'auto))
             (valign (%keyword-value :valign opts 'top))
            ) ;
        (%validate-hstack-spacing spacing)
        (unless (memq valign '(top center bottom))
          (value-error "make-hstack: valign must be 'top, 'center or 'bottom")
        ) ;unless
        (when (pair? widths-spec)
          (unless (= (length widths-spec) (length raw-children))
            (value-error "make-hstack: length of widths does not match number of children")
          ) ;unless
        ) ;when
        (let* ((children (map %validate-hstack-child raw-children))
               (comp (make-component
                       :render
                       (lambda (self width) (%hstack-render children spacing widths-spec valign width))
                       :handle-input
                       (lambda (self key) (%hstack-handle-input children key))
                       :invalidate (lambda (self) (%hstack-invalidate children))
                     ) ;make-component
               ) ;comp
              ) ;
          (component-data-set! comp (list 'hstack children))
          comp
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
