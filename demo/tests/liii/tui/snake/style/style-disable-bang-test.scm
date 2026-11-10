;; style-disable!
;; 关闭全局颜色样式开关
;;
;; 语法
;; ----
;; (style-disable!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; #t\n恒返回 #t

(import (scheme base)
  (liii check)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

(style-enable!)
(check (style-disable!) => #t)
(check-false (style-enabled?))

(check-report)
