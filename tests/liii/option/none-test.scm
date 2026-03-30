(import (liii check)
        (liii option)
) ;import

(check-set-mode! 'report-failed)

;; none
;; 创建空的 option 对象。
;;
;; 语法
;; ----
;; (none)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; 空的 option 对象，内部表示为 (cons #f 'N)。
;;
;; 说明
;; ----
;; 用于表示缺失值或空值。

(check-true (option? (none)))
(check-true (option-empty? (none)))

(check-report)
