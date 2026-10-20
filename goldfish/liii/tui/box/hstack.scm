;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii tui box hstack)
  (export make-hstack hstack?)
  (import (liii base)
    (liii error)
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

    (define (%render-hstack-row padded-cols col-widths num-cols space-str r)
      (let loop
        ((c 0) (parts '()))
        (if (>= c num-cols)
          (apply string-append (reverse parts))
          (let* ((col-w (list-ref col-widths c))
                 (cell (list-ref (list-ref padded-cols c) r))
                 (fitted (%fit-cell cell col-w))
                ) ;
            (if (> c 0)
              (loop (+ c 1) (cons fitted (cons space-str parts)))
              (loop (+ c 1) (cons fitted parts))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
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
                (let loop
                  ((r 0) (acc '()))
                  (if (>= r max-h)
                    (reverse acc)
                    (let ((row-str (%render-hstack-row padded-cols col-widths num-cols space-str r)))
                      (loop (+ r 1) (cons row-str acc))
                    ) ;let
                  ) ;if
                ) ;let
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%hstack-handle-input children key)
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
