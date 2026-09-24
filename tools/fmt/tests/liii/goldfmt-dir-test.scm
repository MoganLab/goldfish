(import (liii check)
  (scheme file)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
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
      (path->string (path-parent (path-parent (path (getcwd)))))
      (getcwd)
    ) ;if
  ) ;if
) ;define

(define gf-bin
  (string-append project-root
    (if (os-windows?) "/bin/gf.exe" "/bin/gf")))

(run-set! 'gf gf-bin)

(define (run-gf . args)
  (let-values (((out err code) (run-values (cons 'gf args) :cwd project-root)))
    code
  ) ;let-values
) ;define

(define (run-gf-values . args)
  (run-values (cons 'gf args) :cwd project-root :stdout 'capture :stderr 'stdout)
) ;define



(define sandbox-dir
  (path->string (path-join (path-temp-dir)
                  (string-append "goldfmt-dir-test-" (number->string (getpid)))
                ) ;path-join
  ) ;path->string
) ;define

(define dir-a (path->string (path-join (path sandbox-dir) "dir_a")))
(define dir-b (path->string (path-join (path sandbox-dir) "dir_b")))
(define file-a (path->string (path-join (path dir-a) "a.scm")))
(define file-b (path->string (path-join (path dir-b) "b.scm")))

(dynamic-wind (lambda ()
                (remove-tree sandbox-dir)
                (mkdir sandbox-dir)
                (mkdir dir-a)
                (mkdir dir-b)
              ) ;lambda
  (lambda ()
    (define unformatted-a "( define   a   1 )\n")
    (define unformatted-b "( define   b   2 )\n")
    (define formatted-a "(define a 1)\n")
    (define formatted-b "(define b 2)\n")

    (path-write-text (path file-a) unformatted-a)
    (path-write-text (path file-b) unformatted-b)

    ;; 1. 未格式化时，针对 dir_a 运行 fmt --check 应报错（退出码 1）
    (let ((code (run-gf "fmt" "--check" dir-a)))
      (check (not (zero? code)) => #t)
    ) ;let

    ;; 2. 格式化单独的目录 dir_a
    (let ((code (run-gf "fmt" dir-a)))
      (check (zero? code) => #t)
    ) ;let

    ;; 3. 验证只有 dir_a 里面的文件被格式化，dir_b 里的文件未被修改
    (check (path-read-text (path file-a)) => formatted-a)
    (check (path-read-text (path file-b)) => unformatted-b)

    ;; 4. 再次对 dir_a 运行 fmt --check 应通过（退出码 0）
    (let ((code (run-gf "fmt" "--check" dir-a)))
      (check (zero? code) => #t)
    ) ;let

    ;; 5. 对仍然未格式化的 dir_b 运行 fmt --check 应报错（退出码 1）
    (let ((code (run-gf "fmt" "--check" dir-b)))
      (check (not (zero? code)) => #t)
    ) ;let

    ;; 6. 对未格式化的单文件运行 fmt --check 应报错（退出码 1）
    (let ((code (run-gf "fmt" "--check" file-b)))
      (check (not (zero? code)) => #t)
    ) ;let

    ;; 7. 格式化单文件 file_b
    (let ((code (run-gf "fmt" file-b)))
      (check (zero? code) => #t)
    ) ;let
    (check (path-read-text (path file-b)) => formatted-b)

    ;; 8. 格式化后对单文件运行 fmt --check 应通过（退出码 0）
    (let ((code (run-gf "fmt" "--check" file-b)))
      (check (zero? code) => #t)
    ) ;let

    ;; 9. 对已格式化的 dir_b 运行 fmt --check 应通过（退出码 0）
    (let ((code (run-gf "fmt" "--check" dir-b)))
      (check (zero? code) => #t)
    ) ;let

    ;; 10. 单文件包含括号错误：应报错、退出码非 0，并提示 gf fix
    (let* ((bad-file (path->string (path-join (path sandbox-dir) "bad.scm"))))
      (path-write-text (path bad-file) "(define x 1))\n")
      (let-values (((out err code) (run-gf-values "fmt" bad-file)))
        (check (not (zero? code)) => #t)
        (check-true (string-contains? out "Failed: "))
        (check-true (string-contains? out "unexpected close paren"))
        (check-true (string-contains? out "Hint: try `gf fix ")))
      (let-values (((out err code) (run-gf-values "fmt" "--dry-run" bad-file)))
        (check (not (zero? code)) => #t)
        (check-true (string-contains? out "Failed: "))
        (check-true (string-contains? out "unexpected close paren"))
        (check-true (string-contains? out "Hint: try `gf fix "))))


    ;; 11. 目录批量格式化容错：遇到错误文件不中断，正常文件被格式化，退出码为 1
    (let* ((dir-c (path->string (path-join (path sandbox-dir) "dir_c")))
           (good-file (path->string (path-join (path dir-c) "good.scm")))
           (bad-file (path->string (path-join (path dir-c) "bad.scm"))))
      (mkdir dir-c)
      (path-write-text (path good-file) "( define   good   1 )\n")
      (path-write-text (path bad-file) "(define bad 2))\n")
      (let-values (((out err code) (run-gf-values "fmt" dir-c)))
        (check (not (zero? code)) => #t)
        (check-true (string-contains? out "Files failed: 1"))
        (check-true (string-contains? out "Failed: "))
        (check-true (string-contains? out "unexpected close paren"))
        (check-true (string-contains? out "Hint: try `gf fix "))
        ;; 验证 good-file 依然被成功格式化
        (check (path-read-text (path good-file)) => "(define good 1)\n")))
  ) ;lambda
  (lambda ()
    (remove-tree sandbox-dir)
  ) ;lambda
) ;dynamic-wind

(check-report)
