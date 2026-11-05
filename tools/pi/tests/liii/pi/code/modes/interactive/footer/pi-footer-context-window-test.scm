;; pi-footer-context-window
;; 读取页脚当前记录的上下文窗口容量（Token 数）。
;;
;; 语法
;; ----
;; (pi-footer-context-window comp)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏页脚组件实例。
;;
;; 返回值
;; ----
;; number
;; 上下文窗口容量（Token 数）；未显式配置时为默认容量 1000000。

(import (scheme base)
  (liii check)
  (liii pi code core context-usage)
  (liii pi code core session-stats)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认容量
(check (pi-footer-context-window (make-pi-footer)) => 1000000)

;; 2. 显式指定容量
(check (pi-footer-context-window (make-pi-footer :context-window 1048576)) => 1048576)

;; 3. 上报上下文占用后：容量取自上下文 record
(let ((ft (make-pi-footer)))
  (pi-footer-set-token-stats! ft (make-pi-session-token-stats 100 50 2000 0)
    95.2 (make-pi-context-usage 12.3 1048576 #t))
  (check (pi-footer-context-window ft) => 1048576))

;; 4. 上下文 record 缺少窗口时：沿用最近一次已知容量，不退化为 0
(let ((ft (make-pi-footer :context-window 1048576)))
  (pi-footer-set-token-stats! ft (make-pi-session-token-stats 100 50 2000 0)
    95.2 (make-pi-context-usage 12.3 #f #t))
  (check (pi-footer-context-window ft) => 1048576))

(check-report)
