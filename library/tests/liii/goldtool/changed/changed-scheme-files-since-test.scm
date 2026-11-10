;; changed-scheme-files-since
;; 列出相对某个版本（rev）已变更的 Scheme 源文件路径。
;; 默认只匹配 .scm 扩展名，可通过扩展名参数覆盖匹配规则。
;;
;; 语法
;; ----
;; (changed-scheme-files-since since . maybe-path)
;; (changed-scheme-files-since since path extensions)
;;
;; 参数
;; ----
;; since : string?
;; 比较基准的 git 版本号或分支名。
;; maybe-path : string? | #f (可选)
;; 路径作用域，限定只返回该路径下的变更文件。
;; extensions : list of string? (可选，默认 (" .scm"))
;; 匹配的扩展名列表。
;;
;; 返回值
;; ------
;; list of string?
;; 变更的 Scheme 文件相对路径列表；无变更时返回空列表。

(import (liii check) (liii goldtool changed) (liii list) (liii os) (liii path))

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
                  (string-append "goldtool-changed-scheme-test-" (number->string (getpid)))
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

    ;; 默认只匹配 .scm
    (let ((files (changed-scheme-files-since "HEAD"))) ;
      (check (contains? "a.scm" files) => #t)
      (check (contains? "sub/c.scm" files) => #t)
      (check (contains? "b.txt" files) => #f)
    ) ;let

    ;; 限定路径作用域
    (check (changed-scheme-files-since "HEAD" "sub") => '("sub/c.scm"))

    ;; 自定义扩展名列表
    (let ((files (changed-scheme-files-since "HEAD" #f '(".txt")))) ;
      (check (contains? "b.txt" files) => #t)
      (check (contains? "a.scm" files) => #f)
    ) ;let

    ;; 自定义扩展名列表可以同时匹配多种扩展名
    (let ((files (changed-scheme-files-since "HEAD" #f '(".scm" ".txt")))) ;
      (check (contains? "a.scm" files) => #t)
      (check (contains? "b.txt" files) => #t)
    ) ;let
  ) ;lambda
  (lambda () (chdir original-cwd) (remove-tree repo-dir))
) ;dynamic-wind

(check-report)
