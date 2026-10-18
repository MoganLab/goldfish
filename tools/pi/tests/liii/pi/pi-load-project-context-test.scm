(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-load-project-context
;; 自动加载当前工作目录下的 AGENTS.md / CLAUDE.md 等项目说明文件。
(define repo-root
  (if (file-exists? "CLAUDE.md")
    "."
    (if (file-exists? "../../CLAUDE.md") "../.." ".")
  ) ;if
) ;define

(let ((ctx (pi-load-project-context repo-root)))
  (check-true (pair? ctx)))

(check-report)
