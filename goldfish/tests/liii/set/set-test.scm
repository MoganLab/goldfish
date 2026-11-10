;; set
;; 创建一个新的 set。
;;
;; 语法
;; ----
;; (set element ...)
;;
;; 参数
;; ----
;; element ... : any
;; 初始元素。
;;
;; 返回值
;; ----
;; set
;; 返回包含指定元素的 set。
;;
;; 错误处理
;; ----
;; 无异常抛出

(import (liii check) (liii set))

(check-set-mode! 'report-failed)

(check-true (set? (set 1 2 3)))
(check-true (set-contains? (set 1) 1))

(check-report)
