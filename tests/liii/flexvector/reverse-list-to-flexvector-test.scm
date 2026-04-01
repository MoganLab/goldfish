(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; reverse-list->flexvector
;; 将列表反向转换为可变长向量。
;;
;; 语法
;; ----
;; (reverse-list->flexvector list)
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
;; 将列表以反向顺序创建为 flexvector。

(check (flexvector->list (reverse-list->flexvector '(a b c))) => '(c b a))
(check (flexvector->list (reverse-list->flexvector '())) => '())
(check (flexvector->vector (reverse-list->flexvector '(1 2 3))) => #(3 2 1))

(check-report)
