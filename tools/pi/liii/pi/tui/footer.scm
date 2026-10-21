;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
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

(define-library (liii pi tui footer)
  (export make-pi-footer pi-footer? format-cwd-for-footer)
  (import (scheme base)
    (scheme process-context)
    (liii base)
    (liii string)
    (liii os)
    (liii tui text-width)
    (liii tui component)
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

    (define (format-cwd-for-footer cwd home)
      (if (and home (string? home) (not (string-null? home)))
        (if (string=? cwd home)
          "~"
          (if (string-starts? cwd (string-append home "/"))
            (string-append "~" (substring cwd (string-length home) (string-length cwd)))
            cwd
          ) ;if
        ) ;if
        cwd
      ) ;if
    ) ;define

    (define (pi-footer? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'pi-footer))
        ) ;let
      ) ;and
    ) ;define

    (define (%render-footer cwd home branch provider model stats width)
      (let* ((formatted-cwd (format-cwd-for-footer cwd home))
             (line1
               (if (and branch (not (string-null? branch)))
                 (string-append formatted-cwd " (" branch ")")
                 formatted-cwd
               ) ;if
             ) ;line1
             (right-part
               (if (and provider (not (string-null? provider)))
                 (string-append "(" provider ") " model)
                 model
               ) ;if
             ) ;right-part
             (left-part stats)
             (left-w (display-width left-part))
             (right-w (display-width right-part))
             (min-padding 2)
             (line2
               (if (<= (+ left-w min-padding right-w) width)
                 (let ((pad-count (- width left-w right-w)))
                   (string-append left-part (make-string pad-count #\space) right-part)
                 ) ;let
                 (if (<= left-w width)
                   (let ((rem-w (- width left-w min-padding)))
                     (if (> rem-w 0)
                       (string-append left-part "  " (truncate-to-width right-part rem-w ""))
                       left-part
                     ) ;if
                   ) ;let
                   (truncate-to-width left-part width "...")
                 ) ;if
               ) ;if
             ) ;line2
             (c-dim (lambda (s) (string-append "\x1b;[38;2;102;102;102m" s "\x1b;[0m")))
             (res1 (c-dim (truncate-to-width line1 width "...")))
             (res2 (c-dim line2))
            ) ;
        (list res1 res2)
      ) ;let*
    ) ;define

    (define (make-pi-footer . args)
      (let* ((home (or (%keyword-value :home args #f)
                     (get-environment-variable "HOME")
                     (get-environment-variable "USERPROFILE")
                   ) ;or
             ) ;home
             (cwd (%keyword-value :cwd args (getcwd)))
             (branch (%keyword-value :branch args #f))
             (provider (%keyword-value :provider args "google"))
             (model (%keyword-value :model args "gemini-3.8-flash • medium"))
             (stats (%keyword-value :stats args "0.0%/1.0M (auto)"))
             (comp (make-component
                     :render
                     (lambda (self width)
                       (%render-footer cwd home branch provider model stats width)
                     ) ;lambda
                   ) ;make-component
             ) ;comp
            ) ;
        (component-data-set! comp (list 'pi-footer cwd branch provider model stats))
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
