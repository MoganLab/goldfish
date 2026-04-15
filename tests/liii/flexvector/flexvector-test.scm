(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector
;; 创建一个新的可变长向量。
;;
;; 语法
;; ----
;; (flexvector element ...)
;;
;; 参数
;; ----
;; element ... : any
;; 初始元素（可选）。
;;
;; 返回值
;; -----
;; 返回包含指定元素的新 flexvector。
;;
(check (flexvector-length (flexvector 1 2 3))
  =>
  3
) ;check


(check-report)
