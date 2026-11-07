;; pi-load-project-context
;; 加载项目上下文文件并注入会话提示
;;
;; 语法
;; ----
;; (pi-load-project-context ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check) (liii string) (liii pi code core prompt context))

(check-set-mode! 'report-failed)

;; pi-load-project-context
;; 自动加载当前工作目录下的 AGENTS.md / CLAUDE.md 等项目说明文件。
;; 使用 xmake.lua 探测仓库根目录，避免误判子目录下的说明文件。

(define repo-root
  (cond ((file-exists? "xmake.lua") ".")
        ((file-exists? "../../xmake.lua") "../..")
        (else ".")
  ) ;cond
) ;define

(let ((ctx (pi-load-project-context repo-root)))
  (check-true (pair? ctx))
  (check-true (string-contains? (cdr (assoc 'path ctx)) "AGENTS.md"))
  (check-true (string-contains? (cdr (assoc 'content ctx)) "Goldfish 仓库工作指南")
  ) ;check-true
) ;let

(check (pi-load-project-context "/non-existent-dir-for-pi-test") => #f)

(check-report)
