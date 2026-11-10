;; fix-changed-since
;; 只处理自指定版本以来变更过的文件，并输出汇总信息。
;; 限定路径不存在时报告错误并以非零状态退出。
;;
;; 语法
;; ----
;; (fix-changed-since since path-str dry-run extensions
;;   repair-source-fn repair-source*-fn)
;;
;; 参数
;; ----
;; since : string? 版本引用
;; path-str : string? 限定处理的路径，空串表示整个仓库
;; dry-run : boolean? 是否为预览模式
;; extensions : list? 参与处理的文件后缀名列表
;; repair-source-fn : procedure? 源码修正函数（预览模式使用）
;; repair-source*-fn : procedure? 返回 (修正后源码 是否跳过) 的修正函数
;;
;; 返回值
;; ------
;; boolean? 处理完成返回 #t。

(import (scheme base)
  (liii check)
  (liii base)
  (liii os)
  (liii path)
  (liii string)
  (liii goldfix)
  (liii goldfix batch)
) ;import

(check-set-mode! 'report-failed)

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

;; 在预览模式下运行 fix-changed-since，返回 (输出文本 返回值)

(define (run-fix-changed-since extensions)
  (let ((result #f))
    (let* ((out
             (with-output-to-string
               (lambda ()
                 (set! result
                   (fix-changed-since "HEAD"
                     ""
                     #t
                     extensions
                     (lambda (src) (string-append src ")"))
                     (lambda (src) (values (string-append src ")") #f))
                   ) ;fix-changed-since
                 ) ;set!
               ) ;lambda
             ) ;with-output-to-string
           ) ;out
          ) ;
      (list out result)
    ) ;let*
  ) ;let
) ;define

(define original-cwd (getcwd))

(define repo-dir
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldfix-changed-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () (remove-tree repo-dir) (mkdir repo-dir) (chdir repo-dir))
  (lambda ()
    (must "git init -q")
    (must "git config user.email goldfish-test@example.com")
    (must "git config user.name Goldfish Test")
    (path-write-text (path "a.scm") "(define a 1)\n")
    (path-write-text (path "b.txt") "old\n")
    (must "git add .")
    (must "git commit -q -m initial")

    ;; 修改已提交的 .scm 文件，使其成为"自 HEAD 以来变更过"的文件
    (path-write-text (path "a.scm") "(define a 2\n")

    ;; 测试：预览模式输出预览结果与汇总信息，不修改文件
    (let ((parts (run-fix-changed-since '(".scm"))))
      (check (cadr parts) => #t)
      (check-true (string-contains? (car parts) "Previewing: a.scm"))
      (check-true (string-contains? (car parts) "Total files fixed: 1"))
      (check (path-read-text (path "a.scm")) => "(define a 2\n")
    ) ;let

    ;; 测试：后缀名不匹配时没有文件需要处理
    (let ((parts (run-fix-changed-since '(".sld"))))
      (check (cadr parts) => #t)
      (check-true (string-contains? (car parts) "No changed Scheme files since HEAD"))
    ) ;let
  ) ;lambda
  (lambda () (chdir original-cwd) (remove-tree repo-dir))
) ;dynamic-wind

(check-report)
