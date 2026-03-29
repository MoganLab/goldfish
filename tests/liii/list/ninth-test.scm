(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; ninth 函数测试
;;
;; 语法
;; ----
;; (ninth list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含九个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第九个元素。
;;
;; 示例
;; ----
;; (ninth '(1 2 3 4 5 6 7 8 9 10)) => 9

(check (ninth '(1 2 3 4 5 6 7 8 9 10)) => 9)

(check-report)
