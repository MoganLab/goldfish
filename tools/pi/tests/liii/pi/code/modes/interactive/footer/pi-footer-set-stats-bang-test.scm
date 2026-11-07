;; pi-footer-set-stats!
;; 更新底部状态栏的统计字符串并标记重绘。
;;
;; 语法
;; ----
;; (pi-footer-set-stats! comp stats)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏组件实例。
;; stats : string
;; 新的统计字符串。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 更新 stats 并验证获取
(let ((ft (make-pi-footer)))
  (check (pi-footer-stats ft) => "0.0%/1.0M (auto)")
  (pi-footer-set-stats! ft "↑1.2k ↓45 R15k CH98.5% 2.5%/1.0M (auto)")
  (check (pi-footer-stats ft) => "↑1.2k ↓45 R15k CH98.5% 2.5%/1.0M (auto)")
) ;let

;; 验证渲染输出包含新的 stats 字符串
(let* ((ft (make-pi-footer))
       (_ (pi-footer-set-stats! ft "CH95.0%"))
       (lines (component-render ft 80))
      ) ;
  (check-true (string? (cadr lines)))
) ;let*

(check-report)
