;; pi-footer-set-context-unknown!
;; 将页脚上下文占比标记为未知（压缩后上下文重建，显示 "?"）。
;;
;; 语法
;; ----
;; (pi-footer-set-context-unknown! comp)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏页脚组件实例。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii pi code core context-usage)
  (liii pi code core session-stats)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. 已知占比先显示百分比，标记未知后显示 "?"，窗口容量沿用
(let ((ft (make-pi-footer)) (tokens (make-pi-session-token-stats 100 50 2000 0)))
  (pi-footer-set-token-stats! ft
    tokens
    95.2
    (make-pi-context-usage 12.3 1048576 #t)
  ) ;pi-footer-set-token-stats!
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% 12.3%/1.0M (auto)")
  (pi-footer-set-context-unknown! ft)
  (check (pi-footer-stats ft) => "↑100 ↓50 R2.0k CH95.2% ?/1.0M (auto)")
  (check (pi-footer-context-window ft) => 1048576)
) ;let

;; 2. 未设置过上下文时标记未知：仍按默认容量显示 "?"
(let ((ft (make-pi-footer)))
  (pi-footer-set-context-unknown! ft)
  (check (pi-footer-stats ft) => "?/1.0M (auto)")
) ;let

(check-report)
