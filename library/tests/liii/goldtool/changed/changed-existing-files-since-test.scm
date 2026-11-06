;; changed-existing-files-since
;; 列出相对某个版本（rev）已变更且当前仍然存在的文件路径。
;; 已被删除的文件不计入结果。
;;
;; 语法
;; ----
;; (changed-existing-files-since since . maybe-path)
;;
;; 参数
;; ----
;; since : string?
;; 比较基准的 git 版本号或分支名。
;; maybe-path : string? (可选)
;; 路径作用域，限定只返回该路径下的变更文件。
;;
;; 返回值
;; ------
;; list of string?
;; 仍然存在的变更文件相对路径列表；无变更时返回空列表。

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
                  (string-append "goldtool-changed-existing-test-" (number->string (getpid)))
                ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () (remove-tree repo-dir) (mkdir repo-dir) (chdir repo-dir))
  (lambda ()
    (must "git init -q")
    (must "git config user.email goldfish-test@example.com")
    (must "git config user.name Goldfish Test")
    (path-write-text (path "keep.scm") "(define keep 1)\n")
    (path-write-text (path "gone.scm") "(define gone 1)\n")
    (must "git add .")
    (must "git commit -q -m initial")

    ;; 修改保留文件，删除另一个文件
    (path-write-text (path "keep.scm") "(define keep 2)\n")
    (path-unlink (path "gone.scm") #t)

    (let ((files (changed-existing-files-since "HEAD"))) ;
      ;; 仍然存在的变更文件被返回
      (check (contains? "keep.scm" files) => #t)
      ;; 已删除的文件被过滤
      (check (contains? "gone.scm" files) => #f)
    ) ;let

    ;; 限定路径作用域
    (check (changed-existing-files-since "HEAD" ".") => '("keep.scm"))
  ) ;lambda
  (lambda () (chdir original-cwd) (remove-tree repo-dir))
) ;dynamic-wind

(check-report)
