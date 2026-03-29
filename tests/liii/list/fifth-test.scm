(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; fifth 函数测试
;;
;; 语法
;; ----
;; (fifth list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含五个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第五个元素。
;;
;; 示例
;; ----
;; (fifth '(1 2 3 4 5 6 7 8 9 10)) => 5

(check (fifth '(1 2 3 4 5 6 7 8 9 10)) => 5)

(check-report)
