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

(define-library (liii tui terminal size)
  (export terminal-detect-size)
  (import (liii base) (liii error) (scheme process-context))
  (begin

    (define (%parse-positive-int str default-val)
      (if (and (string? str) (not (string=? str "")))
        (let ((num (string->number str 10)))
          (if (and (integer? num) (exact? num) (> num 0)) num default-val)
        ) ;let
        default-val
      ) ;if
    ) ;define

    (define (terminal-detect-size . args)
      (let ((env-fn
              (if (and (pair? args) (procedure? (car args)))
                (car args)
                get-environment-variable
              ) ;if
            ) ;env-fn
           ) ;
        (let* ((col-str (env-fn "COLUMNS"))
               (line-str (env-fn "LINES"))
               (w (%parse-positive-int col-str 80))
               (h (%parse-positive-int line-str 24))
              ) ;
          (list w h)
        ) ;let*
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
