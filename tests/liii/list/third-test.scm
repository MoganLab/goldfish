(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; third 函数测试
;;
;; 语法
;; ----
;; (third list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含三个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第三个元素。
;;
;; 示例
;; ----
;; (third '(1 2 3 4 5 6 7 8 9 10)) => 3

(check (third '(1 2 3 4 5 6 7 8 9 10)) => 3)

(check-catch 'wrong-type-arg (third '(1 2)))

(check-report)
