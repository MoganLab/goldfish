;; %make-pi-app
;; 构造 <pi-app> 状态记录底层的底层构造函数。
;;
;; 语法
;; ----
;; (%make-pi-app ...)
;;
;; 参数
;; ----
;; 详见底层 record 字段定义。
;;
;; 返回值
;; ----
;; pi-app?
;; 应用状态实例。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-true (pi-app? app))
) ;let

(check-report)
