;; style-enabled?
;; 判断全局颜色样式开关当前是否开启
;;
;; 语法
;; ----
;; (style-enabled?)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; boolean?\n颜色开启且未设置 NO_COLOR 时返回 #t

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 样式默认关闭
(check (style-enabled?) => #f)

;; style-enable! 开启后生效
(style-enable!)
(check (style-enabled?) => #t)

;; style-disable! 关闭后失效
(style-disable!)
(check (style-enabled?) => #f)

(check-report)
