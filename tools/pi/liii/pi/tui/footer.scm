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
    (liii string)
    (liii os)
    (liii path)
    (liii tui text-width)
    (liii tui component)
    (liii pi tui common)
  ) ;import
  (begin

    ;; 用户主目录：复用 (liii path) 的 path-home（含 Windows HOMEDRIVE/HOMEPATH 回退），
    ;; 未设置时返回 #f
    (define (%default-home)
      (catch #t (lambda () (path->string (path-home))) (lambda args #f))
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
             (rem-w (- width left-w min-padding))
             (line2
               (cond
                ((<= (+ left-w min-padding right-w) width)
                 (string-append left-part
                   (make-string (- width left-w right-w) #\space)
                   right-part
                 ) ;string-append
                ) ;
                ((and (<= left-w width) (> rem-w 0))
                 (string-append left-part "  " (truncate-to-width right-part rem-w ""))
                ) ;
                (else (truncate-to-width left-part width "..."))
               ) ;cond
             ) ;line2
             (c-dim (lambda (s) (string-append "\x1b;[38;2;102;102;102m" s "\x1b;[0m")))
             (res1 (c-dim (truncate-to-width line1 width "...")))
             (res2 (c-dim line2))
            ) ;
        (list res1 res2)
      ) ;let*
    ) ;define

    (define (make-pi-footer . args)
      (let* ((home (or (%keyword-value :home args #f) (%default-home)))
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
