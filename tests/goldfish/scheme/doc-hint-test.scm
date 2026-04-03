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
  (let ((unknown-output-path (path-join (path-temp-dir)
                                        (string-append "gf-doc-hint-unknown-"
                                                       (number->string (getpid))
                                                       ".log")))
        (single-library-output-path (path-join (path-temp-dir)
                                               (string-append "gf-doc-hint-import-"
                                                              (number->string (getpid))
                                                              ".log")))
        (multiple-library-output-path (path-join (path-temp-dir)
                                                 (string-append "gf-doc-hint-multi-"
                                                                (number->string (getpid))
                                                                ".log")))
        (nested-library-output-path (path-join (path-temp-dir)
                                               (string-append "gf-doc-hint-nested-"
                                                              (number->string (getpid))
                                                              ".log")))
        (repl-output-path (path-join (path-temp-dir)
                                     (string-append "gf-doc-hint-repl-"
                                                    (number->string (getpid))
                                                    ".log"))))
    (path-unlink unknown-output-path #t)
    (path-unlink single-library-output-path #t)
    (path-unlink multiple-library-output-path #t)
    (path-unlink nested-library-output-path #t)
    (path-unlink repl-output-path #t)
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (check (not (= (run-shell-command
                        (string-append (executable)
                                       " eval \\\"(foo 1)\\\" > "
                                       (path->string unknown-output-path)
                                       " 2>&1"))
                       0))
               => #t)
        (check (not (= (run-shell-command
                        (string-append (executable)
                                       " eval \\\"(path-join \\\\\\\"a\\\\\\\" \\\\\\\"b\\\\\\\")\\\" > "
                                       (path->string single-library-output-path)
                                       " 2>&1"))
                       0))
               => #t)
        (check (not (= (run-shell-command
                        (string-append (executable)
                                       " eval \\\"(remove 1)\\\" > "
                                       (path->string multiple-library-output-path)
                                       " 2>&1"))
                       0))
               => #t)
        (check (not (= (run-shell-command
                        (string-append (executable)
                                       " eval \\\"((display (path-read-text \\\\\\\"/tmp/demo\\\\\\\")) 0)\\\" > "
                                       (path->string nested-library-output-path)
                                       " 2>&1"))
                       0))
               => #t)
        (check (= (run-shell-command
                   (string-append "printf '(path-join \\\"a\\\" \\\"b\\\")\\n,q\\n' | "
                                  (executable)
                                  " repl > "
                                  (path->string repl-output-path)
                                  " 2>&1"))
                  0)
               => #t)
        (let ((unknown-output (path-read-text unknown-output-path))
              (single-library-output (path-read-text single-library-output-path))
              (multiple-library-output (path-read-text multiple-library-output-path))
              (nested-library-output (path-read-text nested-library-output-path))
              (repl-output (path-read-text repl-output-path)))
          (check-true (string-contains? unknown-output "unbound variable foo in (foo 1)"))
          (check-true (string-contains? unknown-output
                                        "Hint: try `gf doc \"foo\"`"))
          (check-true (string-contains? unknown-output
                                        "`gf doc` may show similarly named functions when there is no exact match."))
          (check-true (string-contains? unknown-output
                                        "If it finds nothing similar, try searching the codebase with `git grep \"foo\"`, implement that function yourself, or stop using it."))

          (check-true (string-contains? single-library-output
                                        "unbound variable path-join in (path-join \"a\" \"b\")"))
          (check-true (string-contains? single-library-output
                                        "Hint: function `path-join` exists in library `(liii path)`."))
          (check-true (string-contains? single-library-output
                                        "Please import that library first: `(import (liii path))`."))

          (check-true (string-contains? multiple-library-output
                                        "unbound variable remove in (remove 1)"))
          (check-true (string-contains? multiple-library-output
                                        "Hint: function `remove` exists in multiple visible libraries:"))
          (check-true (string-contains? multiple-library-output
                                        "  (liii list)"))
          (check-true (string-contains? multiple-library-output
                                        "  (liii os)"))
          (check-true (string-contains? multiple-library-output
                                        "Try one of these commands to decide which library to use:"))
          (check-true (string-contains? multiple-library-output
                                        "gf doc liii/list \"remove\""))
          (check-true (string-contains? multiple-library-output
                                        "gf doc liii/os \"remove\""))

          (check-true (string-contains? nested-library-output
                                        "unbound variable path-read-text"))
          (check-true (string-contains? nested-library-output
                                        "((display (path-read-text \"/tmp/demo\")) 0)"))
          (check-true (string-contains? nested-library-output
                                        "Hint: function `path-read-text` exists in library `(liii path)`."))
          (check-true (string-contains? nested-library-output
                                        "Please import that library first: `(import (liii path))`."))

          (check-true (string-contains? repl-output
                                        "unbound variable path-join in (path-join \"a\" \"b\")"))
          (check-true (string-contains? repl-output
                                        "Hint: function `path-join` exists in library `(liii path)`."))
          (check-true (string-contains? repl-output
                                        "Please import that library first: `(import (liii path))`."))
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink unknown-output-path #t)
        (path-unlink single-library-output-path #t)
        (path-unlink multiple-library-output-path #t)
        (path-unlink nested-library-output-path #t)
        (path-unlink repl-output-path #t)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;when

(check-report)
