;; style-enable!
;; 打开全局颜色样式开关
;;
;; 语法
;; ----
;; (style-enable!)
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

(check (style-enable!) => #t)
(check-true (style-enabled?))

(check-report)
