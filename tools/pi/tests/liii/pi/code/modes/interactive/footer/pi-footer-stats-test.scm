;; pi-footer-stats
;; 获取底部状态栏当前设置的统计字符串。
;;
;; 语法
;; ----
;; (pi-footer-stats comp)
;;
;; 参数
;; ----
;; comp : component?
;; 底部状态栏组件实例。
;;
;; 返回值
;; ----
;; string
;; 当前设置的统计字符串。

(import (scheme base) (liii check) (liii pi code modes interactive footer))

(check-set-mode! 'report-failed)

;; 1. 默认统计字符串
(let ((ft (make-pi-footer)))
  (check (pi-footer-stats ft) => "0.0%/1.0M (auto)"))

;; 2. 构造时指定统计字符串
(let ((ft (make-pi-footer :stats "↑100 ↓50 1.2%/200k")))
  (check (pi-footer-stats ft) => "↑100 ↓50 1.2%/200k"))

(check-report)
