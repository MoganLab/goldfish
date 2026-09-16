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

(run-set! 'gf (string-append project-root "/bin/gf"))

(define (run-gf . args)
  (let-values (((out err code) (run-values (cons 'gf args) :cwd project-root)))
    code
  ) ;let-values
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
  ) ;lambda
  (lambda ()
    (remove-tree sandbox-dir)
  ) ;lambda
) ;dynamic-wind

(check-report)
