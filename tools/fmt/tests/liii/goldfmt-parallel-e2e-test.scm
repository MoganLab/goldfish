(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii either)
  (liii sort)
  (liii goldfmt)
  (liii goldfmt lang)
  (liii goldfmt parallel)
  (liii goldfmt cpp)
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

(define project-root
  (if (file-exists? "gfproject.json")
    (getcwd)
    (if (file-exists? "../../gfproject.json")
      (path->string
        (path-parent (path-parent (path (getcwd))))
      ) ;path->string
      (getcwd)
    ) ;if
  ) ;if
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

(define (clang-format-available?)
  (zero? (os-call (string-append (clang-format-binary) " --version")))
) ;define

(define sandbox-dir
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldfmt-parallel-e2e-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define salt (number->string (getpid)))

(define (pad2 n)
  (if (< n 10) (string-append "0" (number->string n)) (number->string n))
) ;define

(define (write-test-file dir i file-salt formatted?)
  (let* ((nm (string-append "f" (pad2 i) ".scm"))
         (var (string-append "x" (pad2 i)))
         (content (if formatted?
                    (string-append "(define " var " " file-salt ")\n")
                    (string-append "( define   " var "   " file-salt " )\n")
                  ) ;if
         ) ;content
        ) ;
    (path-write-text
      (path (path->string (path-join (path dir) nm)))
      content
    ) ;path-write-text
    (path->string (path-join (path dir) nm))
  ) ;let*
) ;define

(define (expected-content i file-salt)
  (string-append "(define x" (pad2 i) " " file-salt ")\n")
) ;define

(define (sorted-lines out)
  (list-sort string<? (string-split out "\n"))
) ;define

(define (test-t3)
  (let* ((dir-c (path->string (path-join (path sandbox-dir) "t3_a")))
         (dir-e (path->string (path-join (path sandbox-dir) "t3_b")))
         (salt-c (string-append salt "c"))
         (salt-e (string-append salt "e"))
        ) ;
    (mkdir dir-c)
    (mkdir dir-e)
    (let loop
      ((i 1))
      (when (<= i 4)
        (write-test-file dir-c i salt-c #f)
        (write-test-file dir-e i salt-e #f)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (path-write-text
      (path (path->string (path-join (path dir-c) "bad.scm")))
      (string-append "(define bad " salt-c "))\n")
    ) ;path-write-text
    (path-write-text
      (path (path->string (path-join (path dir-e) "bad.scm")))
      (string-append "(define bad " salt-e "))\n")
    ) ;path-write-text
    (let ((res-c (run-gf "fmt" "-j" "4" dir-c)) (res-e (run-gf "fmt" "-j" "1" dir-e)))
      (check (either-left? res-c) => #t)
      (check (either-left? res-e) => #t)
      (let* ((err-c (to-left res-c))
             (err-e (to-left res-e))
             (out-c (process-error-stdout err-c))
             (out-e (process-error-stdout err-e))
            ) ;
        (check-true (string-contains? out-c "Files failed: 1"))
        (check-true (string-contains? out-c "Failed: "))
        (check-true (string-contains? out-c "unexpected close paren"))
        (check-true (string-contains? out-c "Hint: try `gf fix "))
        (check (equal? (sorted-lines (string-replace out-c dir-c "DIR"))
                 (sorted-lines (string-replace out-e dir-e "DIR"))
               ) ;equal?
          =>
          #t
        ) ;check
        (check
          (path-read-text
            (path (path->string (path-join (path dir-c) "f01.scm")))
          ) ;path-read-text
          =>
          (expected-content 1 salt-c)
        ) ;check
      ) ;let*
    ) ;let
  ) ;let*
) ;define

(define (test-t5-setup root5 scm-dir stm-dir salt-5)
  (mkdir root5)
  (mkdir scm-dir)
  (mkdir stm-dir)
  (path-write-text (path (string-append root5 "/gfproject.json"))
    "{\"name\": \"test-project\"}\n"
  ) ;path-write-text
  (path-write-text (path (string-append root5 "/gf_fmt.json"))
    "{\"scheme\": {\"suffix\": [\"scm\"], \"path\": [\"scm\"]}, \"stem\": {\"suffix\": [\"stem\"], \"path\": [\"stm\"]}}\n"
  ) ;path-write-text
  (let loop
    ((i 1))
    (when (<= i 6)
      (write-test-file scm-dir i salt-5 #f)
      (path-write-text (path (string-append stm-dir "/g" (pad2 i) ".stem"))
        (string-append "( define   gy" (pad2 i) "   " salt-5 " )\n")
      ) ;path-write-text
      (loop (+ i 1))
    ) ;when
  ) ;let
  (path-write-text (path (string-append stm-dir "/q01.stem"))
    (string-append "(quote sz" salt-5 ")\n")
  ) ;path-write-text
) ;define

(define (test-t5)
  (let* ((root5 (path->string (path-join (path sandbox-dir) "t5_proj")))
         (scm-dir (path->string (path-join (path root5) "scm")))
         (stm-dir (path->string (path-join (path root5) "stm")))
         (salt-5 salt)
        ) ;
    (test-t5-setup root5 scm-dir stm-dir salt-5)
    (let ((res5 (run-gf-in root5 "fmt" "-j" "8")))
      (check (either-right? res5) => #t)
      (let ((out5 (to-right res5)))
        (let ((p-cpp (string-position "=== Formatting C++ files ===" out5))
              (p-stem (string-position "=== Formatting TeXmacs Stem files ===" out5))
              (p-scheme (string-position "=== Formatting Scheme files ===" out5))
             ) ;
          (check-true (and p-cpp p-stem p-scheme (< p-cpp p-stem) (< p-stem p-scheme)))
        ) ;let
        (check-true (string-contains? out5 "Total Scheme files formatted: 6"))
        (check-true (string-contains? out5 "Total TeXmacs Stem files formatted: 7"))
        (check (path-read-text (path (string-append scm-dir "/f01.scm")))
          =>
          (expected-content 1 salt-5)
        ) ;check
        (check (path-read-text (path (string-append stm-dir "/g01.stem")))
          =>
          (string-append "(define gy01 " salt-5 ")\n")
        ) ;check
        (let ((q01 (path-read-text (path (string-append stm-dir "/q01.stem")))))
          (check-true (string-contains? q01 "(quote "))
          (check (string-position "'" q01) => #f)
        ) ;let
      ) ;let
    ) ;let
  ) ;let*
) ;define

(define (test-t6-body dir-p dir-q salt-p salt-q)
  (mkdir dir-p)
  (mkdir dir-q)
  (let loop
    ((i 1))
    (when (<= i 4)
      (let ((nm (string-append "c" (pad2 i) ".cpp")))
        (path-write-text
          (path (path->string (path-join (path dir-p) nm)))
          (string-append "int   a" salt-p " ;\nint   b ;\n")
        ) ;path-write-text
        (path-write-text
          (path (path->string (path-join (path dir-q) nm)))
          (string-append "int   a" salt-q " ;\nint   b ;\n")
        ) ;path-write-text
        (loop (+ i 1))
      ) ;let
    ) ;when
  ) ;let
  (let ((res-p (run-gf "fmt" "-e" "cpp" "-j" "4" dir-p))
        (res-q (run-gf "fmt" "-e" "cpp" "-j" "1" dir-q))
       ) ;
    (check (either-right? res-p) => #t)
    (check (either-right? res-q) => #t)
    (let ((out-p (to-right res-p)) (out-q (to-right res-q)))
      (let ((norm-p (string-replace (string-replace out-p dir-p "DIR") salt-p "SALT"))
            (norm-q (string-replace (string-replace out-q dir-q "DIR") salt-q "SALT"))
           ) ;
        (check (equal? (sorted-lines norm-p) (sorted-lines norm-q)) => #t)
        (check-true (string-contains? norm-p "Total files formatted: 4"))
        (check-true (string-contains? norm-p "  Updated: "))
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (test-t6)
  (when (clang-format-available?)
    (let ((dir-p (path->string (path-join (path sandbox-dir) "t6_a")))
          (dir-q (path->string (path-join (path sandbox-dir) "t6_b")))
          (salt-p salt)
          (salt-q (string-append salt "q"))
         ) ;
      (test-t6-body dir-p dir-q salt-p salt-q)
    ) ;let
  ) ;when
) ;define

(define (test-t8)
  (let* ((dir-i (path->string (path-join (path sandbox-dir) "t8_a")))
         (dir-j (path->string (path-join (path sandbox-dir) "t8_b")))
         (salt-i (string-append salt "i"))
         (salt-j (string-append salt "j"))
        ) ;
    (mkdir dir-i)
    (mkdir dir-j)
    (let loop
      ((i 1))
      (when (<= i 6)
        (write-test-file dir-i i salt-i #f)
        (write-test-file dir-j i salt-j #f)
        (loop (+ i 1))
      ) ;when
    ) ;let
    (let ((res-i (run-gf "fmt" "--check" "-j" "4" dir-i))
          (res-j (run-gf "fmt" "--check" "-j" "1" dir-j))
         ) ;
      (check (either-left? res-i) => #t)
      (check (either-left? res-j) => #t)
      (let* ((err-i (to-left res-i))
             (err-j (to-left res-j))
             (out-i (process-error-stdout err-i))
             (out-j (process-error-stdout err-j))
            ) ;
        (check (equal? (sorted-lines (string-replace out-i dir-i "DIR"))
                 (sorted-lines (string-replace out-j dir-j "DIR"))
               ) ;equal?
          =>
          #t
        ) ;check
        (check-true (string-contains? out-i "FAIL: 6 file(s) need formatting"))
      ) ;let*
    ) ;let
    (let ((res-fmt (run-gf "fmt" "-j" "4" dir-i)))
      (check (either-right? res-fmt) => #t)
      (let ((res-i2 (run-gf "fmt" "--check" "-j" "4" dir-i)))
        (check (either-right? res-i2) => #t)
        (check-true (string-contains? (to-right res-i2) "OK: all files formatted."))
      ) ;let
    ) ;let
  ) ;let*
) ;define

(define (test-t7)
  ;; 单文件 + -j 8
  (let* ((dir-f (path->string (path-join (path sandbox-dir) "t7_single")))
         (salt-f (string-append salt "f"))
        ) ;
    (mkdir dir-f)
    (write-test-file dir-f 1 salt-f #f)
    (let ((res-f (run-gf "fmt" "-j" "8" dir-f)))
      (check (either-right? res-f) => #t)
      (check
        (path-read-text
          (path (path->string (path-join (path dir-f) "f01.scm")))
        ) ;path-read-text
        =>
        (expected-content 1 salt-f)
      ) ;check
    ) ;let
  ) ;let*
  ;; 空目录 + -j 8
  (let ((dir-g (path->string (path-join (path sandbox-dir) "t7_empty"))))
    (mkdir dir-g)
    (let ((res-g (run-gf "fmt" "-j" "8" dir-g)))
      (check (either-right? res-g) => #t)
      (check-true (string-contains? (to-right res-g) "Total files formatted: 0"))
    ) ;let
  ) ;let
) ;define

(dynamic-wind (lambda () (remove-tree sandbox-dir) (mkdir sandbox-dir))
  (lambda () (test-t3) (test-t5) (test-t6) (test-t8) (test-t7))
  (lambda () (remove-tree sandbox-dir))
) ;dynamic-wind

(check-report)
