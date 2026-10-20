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

(define-library (liii tui box box)
  (export make-box box?)
  (import (liii base)
    (liii error)
    (liii string)
    (liii tui component)
    (liii tui text-width)
    (liii tui box border)
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

    (define (box? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'box))
        ) ;let
      ) ;and
    ) ;define

    (define (%validate-pad val name)
      (unless (and (integer? val) (>= val 0))
        (if (integer? val)
          (value-error (string-append "make-box: " name " must be non-negative"))
          (type-error (string-append "make-box: " name " must be an integer"))
        ) ;if
      ) ;unless
    ) ;define

    (define (%repeat-line line count acc)
      (let loop
        ((i 0) (cur acc))
        (if (< i count) (loop (+ i 1) (cons line cur)) cur)
      ) ;let
    ) ;define

    (define (%render-box-lines border c content-w pad-l pad-r pad-t pad-b
              narrow? v-str width
            ) ;%render-box-lines
      (let* ((child-lines (component-render c content-w))
             (empty-line (if narrow?
                           (truncate-to-width (string-append v-str v-str) width "")
                           (border-render-line border "" content-w pad-l pad-r)
                         ) ;if
             ) ;empty-line
             (acc (%repeat-line empty-line pad-t '()))
            ) ;
        (let loop
          ((rem child-lines) (cur acc))
          (if (null? rem)
            (%repeat-line empty-line pad-b cur)
            (let ((row (if narrow?
                         (truncate-to-width (string-append v-str v-str) width "")
                         (border-render-line border (car rem) content-w pad-l pad-r)
                       ) ;if
                  ) ;row
                 ) ;
              (loop (cdr rem) (cons row cur))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%box-render c border pad-t pad-b pad-l pad-r title width)
      (if (<= width 0)
        '()
        (let* ((none? (eq? (border-style-name border) 'none))
               (v-str (if none? "" (border-style-v border)))
               (v-w (if none? 0 (display-width v-str)))
               (h-overhead (+ (* 2 v-w) pad-l pad-r))
               (content-w (max 0 (- width h-overhead)))
               (narrow? (<= width (* 2 v-w)))
               (body-rev (%render-box-lines border c content-w pad-l pad-r pad-t
                           pad-b narrow? v-str width
                         ) ;%render-box-lines
               ) ;body-rev
               (top-line (border-render-top border width title))
               (bot-line (border-render-bottom border width))
               (with-bot (if none? body-rev (cons bot-line body-rev)))
               (full (reverse with-bot))
              ) ;
          (if none? full (cons top-line full))
        ) ;let*
      ) ;if
    ) ;define

    (define (make-box child . opts)
      (let ((c (if (string? child) (make-text child) child)))
        (unless (component? c)
          (type-error "make-box: child must be a component or string")
        ) ;unless
        (let* ((style-opt (%keyword-value :style opts 'single))
               (border (border-style-resolve style-opt))
               (pad (%keyword-value :padding opts 0))
               (pad-t (%keyword-value :padding-top opts pad))
               (pad-b (%keyword-value :padding-bottom opts pad))
               (pad-l (%keyword-value :padding-left opts pad))
               (pad-r (%keyword-value :padding-right opts pad))
               (title (%keyword-value :title opts #f))
              ) ;
          (%validate-pad pad "padding")
          (%validate-pad pad-t "padding-top")
          (%validate-pad pad-b "padding-bottom")
          (%validate-pad pad-l "padding-left")
          (%validate-pad pad-r "padding-right")
          (when (and title (not (string? title)))
            (type-error "make-box: title must be a string or #f")
          ) ;when
          (let ((comp (make-component
                        :render
                        (lambda (self width) (%box-render c border pad-t pad-b pad-l pad-r title width))
                        :handle-input
                        (lambda (self key) (component-handle-input c key))
                        :invalidate (lambda (self) (component-invalidate! c))
                      ) ;make-component
                ) ;comp
               ) ;
            (component-data-set! comp (list 'box c border))
            comp
          ) ;let
        ) ;let*
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
