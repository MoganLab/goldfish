(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector
;; 使用给定元素创建一个新的可变长向量。
;;
;; 语法
;; ----
;; (flexvector element ...)
;;
;; 参数
;; ----
;; element ... : any
;; 可变数量的初始元素。
;;
;; 返回值
;; ----
;; flexvector
;; 包含指定元素的新 flexvector。
;;
;; 描述
;; ----
;; 直接从参数创建 flexvector，元素按参数顺序排列。

(check (flexvector-length (flexvector)) => 0)
(check (flexvector-length (flexvector 1 2 3)) => 3)
(check (flexvector->vector (flexvector 'a 'b 'c)) => #(a b c))
(check (flexvector->list (flexvector 10 20 30)) => '(10 20 30))

(check-report)
