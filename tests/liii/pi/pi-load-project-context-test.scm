(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-load-project-context
;; 自动加载当前工作目录下的 AGENTS.md / CLAUDE.md 等项目说明文件。
(let ((ctx (pi-load-project-context ".")))
  (check-true (pair? ctx)))

(check-report)
