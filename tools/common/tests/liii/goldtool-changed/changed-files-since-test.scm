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

(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check) (liii goldtool-changed) (liii list) (liii os) (liii path))

(check-set-mode! 'report-failed)

(define (contains? item xs)
  (if (member item xs) #t #f)
) ;define

(define (must command)
  (let ((status (os-call command)))
    (unless (zero? status)
      (error "Command failed" command)
    ) ;unless
  ) ;let
) ;define

(define (remove-tree target)
  (cond ((path-file? target) (path-unlink target #t))
        ((path-dir? target)
         (let ((entries (path-list-path target)))
           (let loop
             ((i 0))
             (if (< i (vector-length entries))
               (begin
                 (remove-tree (vector-ref entries i))
                 (loop (+ i 1))
               ) ;begin
               #t
             ) ;if
           ) ;let
         ) ;let
         (path-rmdir target)
        ) ;
  ) ;cond
) ;define

(define original-cwd (getcwd))
(define repo-dir
  (path->string (path-join (path-temp-dir)
                  (string-append "goldtool-changed-test-" (number->string (getpid)))
                ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () (remove-tree repo-dir) (mkdir repo-dir) (chdir repo-dir))
  (lambda ()
    (must "git init -q")
    (must "git config user.email goldfish-test@example.com")
    (must "git config user.name Goldfish Test")
    (mkdir "sub")
    (path-write-text (path "a.scm") "(define a 1)\n")
    (path-write-text (path "b.txt") "old\n")
    (path-write-text (path "sub/c.scm") "(define c 1)\n")
    (must "git add .")
    (must "git commit -q -m initial")

    (path-write-text (path "a.scm") "(define a 2)\n")
    (path-write-text (path "b.txt") "new\n")
    (path-write-text (path "sub/c.scm") "(define c 2)\n")
    (path-write-text (path "untracked.scm") "(define untracked #t)\n")

    (let ((files (changed-files-since "HEAD")))
      (check (contains? "a.scm" files) => #t)
      (check (contains? "b.txt" files) => #t)
      (check (contains? "sub/c.scm" files) => #t)
      (check (contains? "untracked.scm" files) => #f)
    ) ;let

    (let ((files (changed-scheme-files-since "HEAD")))
      (check (contains? "a.scm" files) => #t)
      (check (contains? "b.txt" files) => #f)
      (check (contains? "sub/c.scm" files) => #t)
      (check (contains? "untracked.scm" files) => #f)
    ) ;let

    (check (changed-scheme-files-since "HEAD" "sub") => '("sub/c.scm"))
  ) ;lambda
  (lambda () (chdir original-cwd) (remove-tree repo-dir))
) ;dynamic-wind

(check-report)
