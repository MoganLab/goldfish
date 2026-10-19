(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii either)
  (liii sort)
  (liii goldfmt)
  (liii goldfmt lang)
  (liii goldfmt cpp)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
  (scheme process-context)
) ;import

(check-set-mode! 'report-failed)

;; cpp-extensions 后缀列表包含常见 C/C++ 扩展名
(check-true (and (member ".cpp" cpp-extensions) #t))
(check-true (and (member ".hpp" cpp-extensions) #t))
(check-true (and (member ".h" cpp-extensions) #t))
(check-true (and (member ".c" cpp-extensions) #t))

;; 语言注册表能按扩展名找到 cpp
(check (lang-name (lang-for-extension ".cpp")) => 'cpp)
(check (lang-name (lang-for-extension ".c")) => 'cpp)

(define (remove-tree target)
  (cond ((path-file? target) (path-unlink target #t))
        ((path-dir? target)
         (vector-for-each remove-tree (path-list-path target))
         (path-rmdir target)
        ) ;
        (else #f)
  ) ;cond
) ;define

(define project-root
  (cond ((file-exists? "gfproject.json") (getcwd))
        ((file-exists? "../../gfproject.json")
         (path->string
           (path-parent (path-parent (path (getcwd))))
         ) ;path->string
        ) ;
        (else (getcwd))
  ) ;cond
) ;define

(define gf-bin
  (string-append project-root (if (os-windows?) "/bin/gf.exe" "/bin/gf"))
) ;define

(run-set! 'gf gf-bin)

(define (run-gf . args)
  (run-either (cons 'gf args) :cwd project-root :stdout 'capture :stderr 'stdout)
) ;define

(define (clang-format-available?)
  (zero? (os-call (string-append (clang-format-binary) " --version")))
) ;define

(define (sub-path base name)
  (path->string (path-join (path base) name))
) ;define

(define (write-file file content)
  (path-write-text (path file) content)
) ;define

(define sandbox-dir
  (sub-path (path-temp-dir)
    (string-append "goldfmt-cpp-" (number->string (getpid)))
  ) ;sub-path
) ;define

(define salt (number->string (getpid)))

(define (pad2 n)
  (if (< n 10) (string-append "0" (number->string n)) (number->string n))
) ;define

(define (sorted-lines out)
  (list-sort string<? (string-split out "\n"))
) ;define

(define (check-output-match out1 d1 s1 out2 d2 s2)
  (let ((n1 (string-replace (string-replace out1 d1 "DIR") s1 "SALT"))
        (n2 (string-replace (string-replace out2 d2 "DIR") s2 "SALT"))
       ) ;
    (check (equal? (sorted-lines n1) (sorted-lines n2)) => #t)
  ) ;let
) ;define

(define (write-cpp-files dir1 dir2 n s1 s2)
  (mkdir dir1)
  (mkdir dir2)
  (let loop
    ((i 1))
    (when (<= i n)
      (let ((nm (string-append "c" (pad2 i) ".cpp")))
        (write-file (sub-path dir1 nm) (string-append "int   a" s1 " ;\nint   b ;\n"))
        (write-file (sub-path dir2 nm) (string-append "int   a" s2 " ;\nint   b ;\n"))
        (loop (+ i 1))
      ) ;let
    ) ;when
  ) ;let
) ;define

(define (test-t6-body dir-p dir-q salt-p salt-q)
  (write-cpp-files dir-p dir-q 4 salt-p salt-q)
  (let ((res-p (run-gf "fmt" "-e" "cpp" "-j" "4" dir-p))
        (res-q (run-gf "fmt" "-e" "cpp" "-j" "1" dir-q))
       ) ;
    (check (either-right? res-p) => #t)
    (check (either-right? res-q) => #t)
    (check-output-match (to-right res-p) dir-p salt-p (to-right res-q) dir-q salt-q)
    (for-each
      (lambda (kw) (check-true (string-contains? (to-right res-p) kw)))
      '("Total files formatted: 4" "  Updated: ")
    ) ;for-each
  ) ;let
) ;define

(define (test-t6)
  (when (clang-format-available?)
    (test-t6-body (sub-path sandbox-dir "t6_a")
      (sub-path sandbox-dir "t6_b")
      salt
      (string-append salt "q")
    ) ;test-t6-body
  ) ;when
) ;define

(dynamic-wind (lambda () (remove-tree sandbox-dir) (mkdir sandbox-dir))
  (lambda () (test-t6))
  (lambda () (remove-tree sandbox-dir))
) ;dynamic-wind

(check-report)
