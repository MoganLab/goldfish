(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii either)
  (liii sort)
  (liii goldfmt)
  (liii goldfmt lang)
  (liii goldfmt parallel)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
  (scheme process-context)
) ;import

(check-set-mode! 'report-failed)

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

(define (run-gf-in cwd . args)
  (run-either (cons 'gf args) :cwd cwd :stdout 'capture :stderr 'stdout)
) ;define

(define (run-gf . args)
  (apply run-gf-in project-root args)
) ;define

(define (sub-path base name)
  (path->string (path-join (path base) name))
) ;define

(define (write-file file content)
  (path-write-text (path file) content)
) ;define

(define (read-file file)
  (path-read-text (path file))
) ;define

(define sandbox-dir
  (sub-path (path-temp-dir)
    (string-append "goldfmt-parallel-e2e-" (number->string (getpid)))
  ) ;sub-path
) ;define

(define salt (number->string (getpid)))

(define (pad2 n)
  (if (< n 10) (string-append "0" (number->string n)) (number->string n))
) ;define

(define (expected-content i file-salt)
  (string-append "(define x" (pad2 i) " " file-salt ")\n")
) ;define

(define (write-test-file dir i file-salt formatted?)
  (let ((nm (string-append "f" (pad2 i) ".scm"))
        (expr (if formatted?
                (expected-content i file-salt)
                (string-append "( define   x" (pad2 i) "   " file-salt " )\n")
              ) ;if
        ) ;expr
       ) ;
    (write-file (sub-path dir nm) expr)
  ) ;let
) ;define

(define (prepare-pair-files dir1 dir2 n salt1 salt2)
  (mkdir dir1)
  (mkdir dir2)
  (let loop
    ((i 1))
    (when (<= i n)
      (write-test-file dir1 i salt1 #f)
      (write-test-file dir2 i salt2 #f)
      (loop (+ i 1))
    ) ;when
  ) ;let
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

(define (check-order out a b c)
  (let ((pa (string-position a out))
        (pb (string-position b out))
        (pc (string-position c out))
       ) ;
    (check-true (and pa pb pc (< pa pb pc)))
  ) ;let
) ;define

(define (test-t3)
  (let ((dir-c (sub-path sandbox-dir "t3_a"))
        (dir-e (sub-path sandbox-dir "t3_b"))
        (salt-c (string-append salt "c"))
        (salt-e (string-append salt "e"))
       ) ;
    (prepare-pair-files dir-c dir-e 4 salt-c salt-e)
    (write-file (sub-path dir-c "bad.scm")
      (string-append "(define bad " salt-c "))\n")
    ) ;write-file
    (write-file (sub-path dir-e "bad.scm")
      (string-append "(define bad " salt-e "))\n")
    ) ;write-file
    (let ((res-c (run-gf "fmt" "-j" "4" dir-c)) (res-e (run-gf "fmt" "-j" "1" dir-e)))
      (check (either-left? res-c) => #t)
      (check (either-left? res-e) => #t)
      (let ((out-c (process-error-stdout (to-left res-c)))
            (out-e (process-error-stdout (to-left res-e)))
           ) ;
        (for-each (lambda (kw) (check-true (string-contains? out-c kw)))
          '("Files failed: 1" "Failed: " "unexpected close paren"
            "Hint: try `gf fix ")
        ) ;for-each
        (check-output-match out-c dir-c "" out-e dir-e "")
        (check (read-file (sub-path dir-c "f01.scm")) => (expected-content 1 salt-c))
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (test-t5-setup root5 scm-dir stm-dir salt-5)
  (mkdir root5)
  (mkdir scm-dir)
  (mkdir stm-dir)
  (write-file (sub-path root5 "gfproject.json") "{\"name\": \"test-project\"}\n")
  (write-file (sub-path root5 "gf_fmt.json")
    "{\"scheme\": {\"suffix\": [\"scm\"], \"path\": [\"scm\"]}, \"stem\": {\"suffix\": [\"stem\"], \"path\": [\"stm\"]}}\n"
  ) ;write-file
  (let loop
    ((i 1))
    (when (<= i 6)
      (write-test-file scm-dir i salt-5 #f)
      (write-file (sub-path stm-dir (string-append "g" (pad2 i) ".stem"))
        (string-append "( define   gy" (pad2 i) "   " salt-5 " )\n")
      ) ;write-file
      (loop (+ i 1))
    ) ;when
  ) ;let
  (write-file (sub-path stm-dir "q01.stem")
    (string-append "(quote sz" salt-5 ")\n")
  ) ;write-file
) ;define

(define (test-t5)
  (let* ((root5 (sub-path sandbox-dir "t5_proj"))
         (scm-dir (sub-path root5 "scm"))
         (stm-dir (sub-path root5 "stm"))
        ) ;
    (test-t5-setup root5 scm-dir stm-dir salt)
    (let ((res5 (run-gf-in root5 "fmt" "-j" "8")))
      (check (either-right? res5) => #t)
      (let ((out5 (to-right res5)))
        (check-order out5 "=== Formatting C++ files ==="
          "=== Formatting TeXmacs Stem files ==="
          "=== Formatting Scheme files ==="
        ) ;check-order
        (for-each (lambda (kw) (check-true (string-contains? out5 kw)))
          '("Total Scheme files formatted: 6"
            "Total TeXmacs Stem files formatted: 7")
        ) ;for-each
        (check (read-file (sub-path scm-dir "f01.scm")) => (expected-content 1 salt))
        (check (read-file (sub-path stm-dir "g01.stem"))
          =>
          (string-append "(define gy01 " salt ")\n")
        ) ;check
        (let ((q01 (read-file (sub-path stm-dir "q01.stem"))))
          (check-true (string-contains? q01 "(quote "))
          (check (string-position "'" q01) => #f)
        ) ;let
      ) ;let
    ) ;let
  ) ;let*
) ;define

(define (test-t8)
  (let ((dir-i (sub-path sandbox-dir "t8_a"))
        (dir-j (sub-path sandbox-dir "t8_b"))
        (salt-i (string-append salt "i"))
        (salt-j (string-append salt "j"))
       ) ;
    (prepare-pair-files dir-i dir-j 6 salt-i salt-j)
    (let ((res-i (run-gf "fmt" "--check" "-j" "4" dir-i))
          (res-j (run-gf "fmt" "--check" "-j" "1" dir-j))
         ) ;
      (check (either-left? res-i) => #t)
      (check (either-left? res-j) => #t)
      (let ((out-i (process-error-stdout (to-left res-i)))
            (out-j (process-error-stdout (to-left res-j)))
           ) ;
        (check-output-match out-i dir-i "" out-j dir-j "")
        (check-true (string-contains? out-i "FAIL: 6 file(s) need formatting"))
      ) ;let
    ) ;let
    (check (either-right? (run-gf "fmt" "-j" "4" dir-i)) => #t)
    (let ((res-i2 (run-gf "fmt" "--check" "-j" "4" dir-i)))
      (check (either-right? res-i2) => #t)
      (check-true (string-contains? (to-right res-i2) "OK: all files formatted."))
    ) ;let
  ) ;let
) ;define

(define (test-t7)
  (let ((df (sub-path sandbox-dir "t7_single"))
        (dg (sub-path sandbox-dir "t7_empty"))
        (sf (string-append salt "f"))
       ) ;
    (mkdir df)
    (mkdir dg)
    (write-test-file df 1 sf #f)
    (let ((rf (run-gf "fmt" "-j" "8" df)) (rg (run-gf "fmt" "-j" "8" dg)))
      (check (either-right? rf) => #t)
      (check (either-right? rg) => #t)
      (check (read-file (sub-path df "f01.scm")) => (expected-content 1 sf))
      (check-true (string-contains? (to-right rg) "Total files formatted: 0"))
    ) ;let
  ) ;let
) ;define

(dynamic-wind (lambda () (remove-tree sandbox-dir) (mkdir sandbox-dir))
  (lambda () (test-t3) (test-t5) (test-t8) (test-t7))
  (lambda () (remove-tree sandbox-dir))
) ;dynamic-wind

(check-report)
