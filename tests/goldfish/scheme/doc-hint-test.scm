;
; Copyright (C) 2024-2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii sys)
) ;import

(check-set-mode! 'report-failed)

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(when (not (os-windows?))
  (let ((output-path (path-join (path-temp-dir)
                                (string-append "gf-doc-hint-"
                                               (number->string (getpid))
                                               ".log"))))
    (path-unlink output-path #t)
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (check (not (= (run-shell-command
                        (string-append (executable)
                                       " eval \\\"(foo 1)\\\" > "
                                       (path->string output-path)
                                       " 2>&1"))
                       0))
               => #t)
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output "unbound variable foo in (foo 1)"))
          (check-true (string-contains? output
                                        "Hint: try `gf doc \"foo\"` to look up related documentation."))
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink output-path #t)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;when

(check-report)
