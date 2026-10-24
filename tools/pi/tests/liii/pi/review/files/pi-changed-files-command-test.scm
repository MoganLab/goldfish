(import (liii check)
        (liii string)
        (liii pi review files)
) ;import

(check-set-mode! 'report-failed)

;; pi-changed-files-command
;; 构造 git diff --name-status 命令。
(check (pi-changed-files-command "HEAD" #f) => "git diff --name-status HEAD")
(check (pi-changed-files-command "main" #f) => "git diff --name-status main")
(check (pi-changed-files-command "HEAD" #t) => "git diff --name-status --cached")
(check (pi-changed-files-command "v1.2.3" #f) => "git diff --name-status v1.2.3")

;; 不安全 ref 加单引号，避免 shell 注入
(let ((cmd (pi-changed-files-command "main; rm -rf /tmp/x" #f)))
  (check-true (string-contains? cmd "'main; rm -rf /tmp/x'")))

(check-report)
