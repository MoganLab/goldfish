;; git-branch-from-head-file
;; 从指定 git 目录的 HEAD 文件解析当前分支名称。
;;
;; 语法
;; ----
;; (git-branch-from-head-file git-dir)
;;
;; 参数
;; ----
;; git-dir : string?
;; .git 目录绝对路径。
;;
;; 返回值
;; ----
;; string? or boolean?
;; 解析成功返回分支名称字符串；分离 HEAD 或读取失败返回 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code modes interactive common)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string (path-join (path-temp-dir) "git-branch-head-test"))
) ;define
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR)
) ;when
(path-mkdir TMP-DIR :parents #t :exist-ok #t)

;; 1. 成功解析标准 ref: refs/heads/ 分支
(let ((head-file (string-append TMP-DIR "/HEAD")))
  (call-with-output-file head-file
    (lambda (p) (display "ref: refs/heads/main\n" p))
  ) ;call-with-output-file
  (check (git-branch-from-head-file TMP-DIR) => "main")
) ;let

;; 2. detached HEAD 返回 #f
(let ((head-file (string-append TMP-DIR "/HEAD")))
  (call-with-output-file head-file
    (lambda (p) (display "0123456789abcdef0123456789abcdef01234567\n" p))
  ) ;call-with-output-file
  (check-false (git-branch-from-head-file TMP-DIR))
) ;let

;; 3. 不存在的目录安全返回 #f
(check-false (git-branch-from-head-file "/non/existent/git/dir"))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR)
) ;when

(check-report)
