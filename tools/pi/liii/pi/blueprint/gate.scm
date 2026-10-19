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

(define-library (liii pi blueprint gate)
  (import (scheme base)
    (liii string)
    (liii error)
    (liii either)
    (liii subprocess)
    (liii subprocess process-error)
    (liii pi time)
    (liii pi blueprint types)
  ) ;import
  (export pi-run-blueprint-gate
    pi-blueprint-gate-failure-summary
    pi-blueprint-supported-gate-type?
  ) ;export
  (begin

    (define (pi-blueprint-supported-gate-type? type)
      (or (string=? type "custom_command") (string=? type "user_approval"))
    ) ;define

    (define (%result passed output error duration)
      (list (cons 'passed passed)
        (cons 'output output)
        (cons 'error error)
        (cons 'duration-ms duration)
      ) ;list
    ) ;define

    (define (%error-text tag info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       ((pair? info) (object->string (car info)))
       (else (object->string tag))
      ) ;cond
    ) ;define

    (define (%run-command command timeout)
      (let ((start (pi-current-time-millis)))
        (catch #t
          (lambda ()
            (let ((res (run-either command :timeout timeout :stdout 'capture :stderr 'capture))
                  (duration (- (pi-current-time-millis) start))
                 ) ;
              (if (either-right? res)
                (%result #t (to-right res) #f duration)
                (let* ((err (to-left res))
                       (code (process-error-code err))
                       (msg (process-error-message err))
                      ) ;
                  (%result #f
                    (if (string? (process-error-stdout err)) (process-error-stdout err) "")
                    (if (and (integer? code) (= code -1))
                      (string-append msg " (timed out or killed after " (number->string timeout) "s)")
                      msg
                    ) ;if
                    duration
                  ) ;%result
                ) ;let*
              ) ;if
            ) ;let
          ) ;lambda
          (lambda (tag . info)
            (%result #f "" (%error-text tag info) (- (pi-current-time-millis) start))
          ) ;lambda
        ) ;catch
      ) ;let
    ) ;define

    ;; 执行单个验证门；返回
    ;; ((passed . bool) (output . str) (error . str|#f) (duration-ms . int))。
    (define (pi-run-blueprint-gate gate timeout)
      (unless (and (real? timeout) (> timeout 0))
        (value-error "pi-run-blueprint-gate: :timeout must be a positive number")
      ) ;unless
      (let ((type (pi-gate-type gate)) (command (pi-gate-command gate)))
        (cond ((string=? type "user_approval") (%result #f "" "User approval required" 0))
              ((string=? type "custom_command")
               (if
                 (not
                   (and (string? command) (not (string-null? (string-trim-both command))))
                 ) ;not
                 (%result #f "" "custom_command gate has no command" 0)
                 (%run-command command timeout)
               ) ;if
              ) ;
              (else (%result #f "" (string-append "Unsupported gate type: " type) 0))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-blueprint-gate-failure-summary gate result)
      (let ((err (cdr (assoc 'error result))))
        (string-append "- FAILED: "
          (pi-gate-description gate)
          (if (and (string? err) (not (string-null? err))) (string-append " -- " err) "")
        ) ;string-append
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
