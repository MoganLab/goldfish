(set! *load-path* (cons "tools/fmt" *load-path*))
(set! *load-path* (cons "tools/common" *load-path*))

(import (liii check)
  (liii either)
  (liii sort)
  (liii goldfmt)
  (liii goldfmt lang)
  (liii goldfmt path)
  (liii goldfmt parallel)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (scheme file)
  (scheme process-context)
) ;import

(check-set-mode! 'report-failed)

;; ---- T1: -j/--jobs 解析 ------------------------------------------------

(check (parse-fmt-jobs '("gf" "fmt")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "dir")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "4")) => 4)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "10" "dir")) => 10)
(check (parse-fmt-jobs '("gf" "fmt" "--jobs=8")) => 8)
(check (parse-fmt-jobs '("gf" "fmt" "-j=16")) => 16)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "0")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "-2")) => 0)
(check (parse-fmt-jobs '("gf" "fmt" "-j" "abc" "dir")) => 0)

(check (fmt-jobs) => 1)
(set-fmt-jobs! 4)
(check (fmt-jobs) => 4)
(set-fmt-jobs! 1)
(check (fmt-jobs) => 1)

;; ---- 端到端基础设施 ---------------------------------------------------

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

(define (run-gf . args)
  (run-either (cons 'gf args) :cwd project-root :stdout 'capture :stderr 'stdout)
) ;define

(define sandbox-dir
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldfmt-parallel-test-" (number->string (getpid)))
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

(define (test-dfs-files dir)
  (let ((entries (path-list-path (path dir))))
    (let loop
      ((i 0) (acc '()))
      (if (>= i (vector-length entries))
        acc
        (let ((e (vector-ref entries i)))
          (cond
           ((path-file? e)
            (let ((s (path->string e)))
              (loop (+ i 1) (if (string-ends? s ".scm") (append acc (list s)) acc))
            ) ;let
           ) ;
           ((path-dir? e) (loop (+ i 1) (append acc (test-dfs-files (path->string e)))))
           (else (loop (+ i 1) acc))
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;let
) ;define

(define (test-t0)
  (let* ((d0 (path->string (path-join (path sandbox-dir) "t0_dfs")))
         (sub (path->string (path-join (path d0) "sub")))
         (deep (path->string (path-join (path sub) "deep")))
        ) ;
    (mkdir d0)
    (mkdir sub)
    (mkdir deep)
    (write-test-file d0 1 salt #f)
    (path-write-text (path (string-append d0 "/z.txt")) "not scm\n")
    (write-test-file d0 2 salt #f)
    (write-test-file sub 3 salt #f)
    (write-test-file deep 4 salt #f)
    (path-write-text (path (string-append deep "/n.scm")) "(define n 1)\n")
    (write-test-file sub 5 salt #f)
    (check (collect-files-dfs d0 '(".scm") '()) => (test-dfs-files d0))
    (check (length (collect-files-dfs d0 '(".scm") '("deep"))) => 4)
    (check (length (collect-files-dfs d0 '(".scm") '("sub"))) => 2)
  ) ;let*
) ;define

(define (test-t2-files dir-a dir-b salt-a salt-b n)
  (let loop
    ((i 1))
    (when (<= i n)
      (write-test-file dir-a i salt-a (= 0 (modulo i 2)))
      (write-test-file dir-b i salt-b (= 0 (modulo i 2)))
      (loop (+ i 1))
    ) ;when
  ) ;let
) ;define

(define (check-t2-contents dir-a dir-b salt-a salt-b n)
  (let loop
    ((i 1))
    (if (> i n)
      #t
      (let ((fa
              (path->string (path-join (path dir-a) (string-append "f" (pad2 i) ".scm")))
            ) ;fa
            (fb
              (path->string (path-join (path dir-b) (string-append "f" (pad2 i) ".scm")))
            ) ;fb
           ) ;
        (check (path-read-text (path fa)) => (expected-content i salt-a))
        (check (path-read-text (path fb)) => (expected-content i salt-b))
        (loop (+ i 1))
      ) ;let
    ) ;if
  ) ;let
) ;define

(define (test-t2-and-t4)
  (let* ((n 30)
         (dir-a (path->string (path-join (path sandbox-dir) "t2_a")))
         (dir-b (path->string (path-join (path sandbox-dir) "t2_b")))
         (salt-a salt)
         (salt-b (string-append salt "z"))
        ) ;
    (mkdir dir-a)
    (mkdir dir-b)
    (test-t2-files dir-a dir-b salt-a salt-b n)
    (let ((res-a (run-gf "fmt" "-j" "8" dir-a)) (res-b (run-gf "fmt" "-j" "1" dir-b)))
      (check (either-right? res-a) => #t)
      (check (either-right? res-b) => #t)
      (let* ((out-a (to-right res-a))
             (out-b (to-right res-b))
             (norm-a (string-replace out-a dir-a "DIR"))
             (norm-b (string-replace out-b dir-b "DIR"))
            ) ;
        (check (equal? (sorted-lines norm-a) (sorted-lines norm-b)) => #t)
        (check-true (string-contains? norm-a "Total files formatted: 30"))
        (check-true (string-contains? norm-a "Files updated: 15"))
        (check-t2-contents dir-a dir-b salt-a salt-b n)
      ) ;let*
    ) ;let
    ;; T4: 缓存测试
    (let ((res2 (run-gf "fmt" "-j" "8" dir-a)))
      (check (either-right? res2) => #t)
      (let ((out2 (to-right res2)))
        (check (string-position "  Updated: " out2) => #f)
        (check (string-position "Formatting: " out2) => #f)
        (check-true (string-contains? out2 "Total files formatted: 30"))
        (check-true (string-contains? out2 "Files updated: 0"))
        (check-true (string-contains? out2 "Files unchanged: 30"))
      ) ;let
    ) ;let
  ) ;let*
) ;define

(define (test-t2b)
  (let* ((dir-d (path->string (path-join (path sandbox-dir) "t2b")))
         (salt-d (string-append salt "d"))
        ) ;
    (mkdir dir-d)
    (let loop
      ((i 1))
      (when (<= i 5)
        (write-test-file dir-d i salt-d (= 0 (modulo i 2)))
        (loop (+ i 1))
      ) ;when
    ) ;let
    (let ((res-d (run-gf "fmt" dir-d)))
      (check (either-right? res-d) => #t)
      (let ((out-d (to-right res-d)))
        (check-true (string-contains? out-d "Total files formatted: 5"))
        (check-true (string-contains? out-d "Files updated: 3"))
        (check
          (path-read-text
            (path (path->string (path-join (path dir-d) "f03.scm")))
          ) ;path-read-text
          =>
          (expected-content 3 salt-d)
        ) ;check
      ) ;let
    ) ;let
  ) ;let*
) ;define

(dynamic-wind (lambda () (remove-tree sandbox-dir) (mkdir sandbox-dir))
  (lambda () (test-t0) (test-t2-and-t4) (test-t2b))
  (lambda () (remove-tree sandbox-dir))
) ;dynamic-wind

(check-report)
