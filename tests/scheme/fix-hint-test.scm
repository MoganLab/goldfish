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
  (let* ((bad-file (path-join (path-temp-dir)
                              (string-append "gf-fix-hint-"
                                             (number->string (getpid))
                                             ".scm")))
         (output-path (path-join (path-temp-dir)
                                 (string-append "gf-fix-hint-"
                                                (number->string (getpid))
                                                ".log"))))
    (path-unlink bad-file #t)
    (path-unlink output-path #t)
    (dynamic-wind
      (lambda ()
        (path-write-text bad-file "(define x 1))\n")
      ) ;lambda
      (lambda ()
        (check (not (= (run-shell-command
                        (string-append (executable)
                                       " "
                                       (path->string bad-file)
                                       " > "
                                       (path->string output-path)
                                       " 2>&1"))
                       0))
               => #t)
        (let ((output (path-read-text output-path))
              (fix-hint (string-append "Hint: try `gf fix "
                                       (path->string bad-file)
                                       "` to repair common parenthesis issues.")))
          (check-true (string-contains? output "unexpected close paren"))
          (check-true (string-contains? output fix-hint))
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink bad-file #t)
        (path-unlink output-path #t)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
