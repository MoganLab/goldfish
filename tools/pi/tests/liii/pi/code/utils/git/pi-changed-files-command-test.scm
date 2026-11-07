;; pi-changed-files-command
;; 构造获取变更文件状态的 git diff 命令字符串。
;;
;; 语法
;; ----
;; (pi-changed-files-command ref staged)
;;
;; 参数
;; ----
;; ref : string?
;; Git 引用名或提交哈希。
;;
;; staged : boolean?
;; 是否比较暂存区。
;;
;; 返回值
;; ----
;; string?
;; 完整的 git diff 命令行字符串。

(import (liii check)
        (liii string)
        (liii pi code utils git)
) ;import

(check-set-mode! 'report-failed)

;; 构造 git diff --name-status 命令。
(check (pi-changed-files-command "HEAD" #f) => "git diff --name-status HEAD")
(check (pi-changed-files-command "main" #f) => "git diff --name-status main")
(check (pi-changed-files-command "HEAD" #t) => "git diff --name-status --cached")
(check (pi-changed-files-command "v1.2.3" #f) => "git diff --name-status v1.2.3")

;; 不安全 ref 加单引号，避免 shell 注入
(let ((cmd (pi-changed-files-command "main; rm -rf /tmp/x" #f)))
  (check-true (string-contains? cmd "'main; rm -rf /tmp/x'")))

(check-report)
