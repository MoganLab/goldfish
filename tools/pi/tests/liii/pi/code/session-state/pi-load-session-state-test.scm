;; pi-load-session-state
;; 从启动时恢复的会话文件读取投影状态。
;;
;; 语法
;; ----
;; (pi-load-session-state parsed cfg)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; cfg : list?
;; Agent 配置 alist。
;;
;; 返回值
;; ----
;; (or alist #f)
;; 投影状态字典，无恢复来源时返回 #f。

(import (liii check) (liii pi code session-state))

(check-set-mode! 'report-failed)

;; 1. --no-session 时不读取状态
(check (pi-load-session-state '((no-session . #t)) '()) => #f)

;; 2. 无恢复来源时返回 #f
(check (pi-load-session-state '() '((agent-dir . "/tmp/pi-agent-load-state")))
  =>
  #f
) ;check

(check-report)
