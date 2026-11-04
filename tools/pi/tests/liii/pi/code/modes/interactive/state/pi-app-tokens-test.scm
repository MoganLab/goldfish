;; pi-app-tokens
;; 获取当前应用实例记录的会话累计 Token 统计 record。
;;
;; 语法
;; ----
;; (pi-app-tokens app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; pi-session-token-stats?
;; 会话全局 Token 统计 record。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-stats)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-true (pi-session-token-stats? (pi-app-tokens app)))
  (check (pi-session-token-stats-total (pi-app-tokens app)) => 0)
) ;let

(check-report)
