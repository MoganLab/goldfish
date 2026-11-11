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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (scheme read)
  (import (scheme base))
  (export read)
  (begin

    (define %s7-read ((rootlet) 'read))

    ;; R7RS 6.13.2 Reading: read
    ;; 若读取时发生语法错误或意外 EOF，read 按 R7RS 规范抛出满足 read-error? 的错误对象。
    (define* (read (port (current-input-port)))
      (catch #t
        (lambda () (%s7-read port))
        (lambda (tag . args)
          (if (memq tag '(read-error string-read-error))
            (let ((msg
                    (if (and (pair? args) (pair? (car args)) (string? (caar args)))
                      (caar args)
                      "read error"
                    ) ;if
                  ) ;msg
                  (irritants
                    (if (and (pair? args) (pair? (car args))) (cdar args) args)
                  ) ;irritants
                 ) ;
              (raise (%make-error-object 'read-error msg irritants))
            ) ;let
            (apply throw tag args)
          ) ;if
        ) ;lambda
      ) ;catch
    ) ;define*

  ) ;begin
) ;define-library
