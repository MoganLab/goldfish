(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; list->flexvector
;; 将列表转换为可变长向量。
;;
;; 语法
;; ----
;; (list->flexvector list)
;;
;; 参数
;; ----
;; list : list
;; 源列表。
;;
;; 返回值
;; ----
;; flexvector
;; 转换后的 flexvector。
;;
;; 描述
;; ----
;; 从列表创建 flexvector。

(check (flexvector->list (list->flexvector '(a b c))) => '(a b c))
(check (flexvector->list (list->flexvector '())) => '())
(check (flexvector->vector (list->flexvector '(1 2 3))) => #(1 2 3))

(check-report)
