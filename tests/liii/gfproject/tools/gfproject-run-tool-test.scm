;; gfproject-run-tool
;; 顶层调度执行已注册的工具。
;;
;; 语法
;; ----
;; (gfproject-run-tool command [opt-gf-lib]) -> integer or #f
;;
;; 参数
;; ----
;; command : string
;; 工具命令名称。
;;
;; opt-gf-lib : string (可选)
;; goldfish 库根路径。
;;
;; 返回值
;; ----
;; integer or #f
;; 工具退出码；若未注册该工具则返回 #f。

(import (liii check)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. 未知工具返回 #f
(check (gfproject-run-tool "non-existent-tool-xyz-98765") => #f)

;; 2. 真实工具（如 version）正常运行返回退出码 0
(let ((res (gfproject-run-tool "version")))
  (check res => 0)
) ;let

(check-report)
