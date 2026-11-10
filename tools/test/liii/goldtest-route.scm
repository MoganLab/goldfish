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

(define-library (liii goldtest-route)
  (import (scheme base)
    (scheme process-context)
    (scheme file)
    (liii string)
    (liii os)
    (liii sys)
    (liii path)
  ) ;import
  (export git-current-branch git-branch-exists? route-test-command)
  (begin

    (define (git-current-branch)
      (let* ((tmp-file
               (path->string
                 (path-join (path-temp-dir)
                   (string-append "gf-test-branch-" (number->string (getpid)) ".txt")
                 ) ;path-join
               ) ;path->string
             ) ;tmp-file
             (cmd (string-append "git rev-parse --abbrev-ref HEAD > " tmp-file " 2>&1"))
             (exit-code (os-call cmd))
            ) ;
        (if (zero? exit-code)
          (let* ((port (open-input-file tmp-file)) (branch (read-line port)))
            (close-input-port port)
            (remove tmp-file)
            branch
          ) ;let*
          (begin
            (when (file-exists? tmp-file)
              (remove tmp-file)
            ) ;when
            #f
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (git-branch-exists? branch)
      (let* ((tmp-file
               (path->string
                 (path-join (path-temp-dir)
                   (string-append "gf-branch-check-" (number->string (getpid)) ".txt")
                 ) ;path-join
               ) ;path->string
             ) ;tmp-file
             (cmd (string-append "git rev-parse --verify " branch " > " tmp-file " 2>&1"))
             (exit-code (os-call cmd))
            ) ;
        (when (file-exists? tmp-file)
          (remove tmp-file)
        ) ;when
        (zero? exit-code)
      ) ;let*
    ) ;define

    (define (route-test-command args all-mode)
      (let ((branch (git-current-branch)) (exe (executable)))
        (cond (all-mode (display "[gf test] Running all unit tests under all domains (--all).\n\n")
                (cons #f #t)
              ) ;all-mode
              ((or (git-branch-exists? "main") (and branch (not (string=? branch "main"))))
               (display "[gf test] Running changed tests since main. Use `gf test --all` to run all.\n\n"
               ) ;display
               (cons "main" #f)
              ) ;
              (else (display "[gf test] Running all tests.\n\n") (cons #f #t))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
