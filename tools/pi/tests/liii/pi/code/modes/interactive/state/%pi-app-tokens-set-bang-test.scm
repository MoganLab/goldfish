;; %pi-app-tokens-set!
;; 设置应用实例的会话累计 Token 统计 record。
;;
;; 语法
;; ----
;; (%pi-app-tokens-set! app tokens)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; tokens : pi-session-token-stats?
;; 新的 Token 统计 record。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-stats)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
      (tokens (make-pi-session-token-stats 100 50 800 0)))
  (%pi-app-tokens-set! app tokens)
  (check (pi-app-tokens app) => tokens)
) ;let

(check-report)
