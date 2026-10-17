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

(define-library (liii pi bash)
  (import (scheme base)
    (liii string)
    (liii os)
    (liii either)
    (liii subprocess)
    (liii pi read)
    (liii pi truncate)
  ) ;import
  (export pi-bash-schema pi-execute-bash)
  (begin

    (define (pi-bash-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "command"
                  (list (cons "type" "string") (cons "description" "Shell command to execute"))
                ) ;cons
            (cons "timeout"
              (list (cons "type" "number")
                (cons "description" "Timeout in seconds (optional, no default timeout)")
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("command"))
      ) ;list
    ) ;define

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

    (define (%format-tail-notice trunc)
      (let* ((by (cdr (assoc 'truncated-by trunc)))
             (total (cdr (assoc 'total-lines trunc)))
             (out-lines (cdr (assoc 'output-lines trunc)))
             (out-bytes (cdr (assoc 'output-bytes trunc)))
             (start-line (+ (- total out-lines) 1))
             (end-line total)
            ) ;
        (cond ((cdr (assoc 'last-line-partial? trunc))
               (string-append "\n\n[Showing last "
                 (pi-format-size out-bytes)
                 " of line "
                 (number->string end-line)
                 ".]"
               ) ;string-append
              ) ;
              ((eq? by 'lines)
               (string-append "\n\n[Showing lines "
                 (number->string start-line)
                 "-"
                 (number->string end-line)
                 " of "
                 (number->string total)
                 ".]"
               ) ;string-append
              ) ;
              (else (string-append "\n\n[Showing lines "
                      (number->string start-line)
                      "-"
                      (number->string end-line)
                      " of "
                      (number->string total)
                      " (50.0KB limit).]"
                    ) ;string-append
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (%truncate-bash-output text)
      (let ((trunc (pi-truncate-tail text)))
        (if (cdr (assoc 'truncated trunc))
          (string-append (cdr (assoc 'content trunc)) (%format-tail-notice trunc))
          text
        ) ;if
      ) ;let
    ) ;define

    (define (%format-bash-result raw-text exit-code timeout)
      (let* ((text (%truncate-bash-output raw-text)) (trimmed-text (string-trim-both text)))
        (cond ((= exit-code 0) (values text #f))
              ((= exit-code -1)
               (let ((msg (string-append "Command timed out after " (number->string timeout) " seconds")
                     ) ;msg
                    ) ;
                 (values (if (string-null? trimmed-text) msg (string-append text "\n\n" msg)) #t)
               ) ;let
              ) ;
              (else
                (let ((msg (string-append "Command exited with code " (number->string exit-code))))
                  (values (if (string-null? trimmed-text) msg (string-append text "\n\n" msg)) #t)
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-execute-bash args . opts)
      (let* ((timed-out? (%keyword-value :timed-out? opts #f))
             (cwd (%keyword-value :cwd opts (getcwd)))
             (def-timeout (%keyword-value :timeout opts 120))
             (command (pi-arg-ref args "command"))
             (arg-timeout (pi-arg-ref args "timeout" #f))
             (timeout (or arg-timeout def-timeout 120))
            ) ;
        (if timed-out?
          (values (string-append "Command timed out after " (number->string timeout) " seconds")
            #t
          ) ;values
          (begin
            (unless (string? command)
              (error 'error "command must be a string")
            ) ;unless
            (let ((res (run-either (list "/bin/bash" "-c" command)
                         :cwd     cwd
                         :stdout  'capture
                         :stderr  'stdout
                         :timeout timeout
                       ) ;run-either
                  ) ;res
                 ) ;
              (if (either-right? res)
                (%format-bash-result (to-right res) 0 timeout)
                (let ((err (to-left res)))
                  (%format-bash-result (process-error-stdout err)
                    (process-error-code err)
                    timeout
                  ) ;%format-bash-result
                ) ;let
              ) ;if
            ) ;let
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
