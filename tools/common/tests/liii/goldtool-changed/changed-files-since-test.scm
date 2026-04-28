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

(define original-cwd (getcwd))
(define repo-dir
  (path->string (path-join (path-temp-dir)
                  (string-append "goldtool-changed-test-" (number->string (getpid)))
                ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda ()
                (os-call (string-append "rm -rf " repo-dir))
                (mkdir repo-dir)
                (chdir repo-dir)
              ) ;lambda
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
  (lambda () (chdir original-cwd) (os-call (string-append "rm -rf " repo-dir)))
) ;dynamic-wind

(check-report)
