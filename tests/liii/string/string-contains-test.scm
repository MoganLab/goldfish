(import (liii check)
        (liii string)
) ;import

;; string-contains
;; 检查字符串是否包含指定子串。
;;
;; 语法
;; ----
;; (string-contains str sub-str)
;;
;; 参数
;; ----
;; str : string?
;; 要检查的源字符串。
;;
;; sub-str : string?
;; 要查找的子串。
;;
;; 返回值
;; ----
;; integer or #f
;; 如果str包含sub-str返回起始位置索引，否则返回#f。
;;
;; 注意
;; ----
;; 空字符串作为sub-str时总是返回0。

(check-true (string-contains "0123456789" "3"))
(check-true (string-contains "0123456789" "34"))
(check-false (string-contains "0123456789" "24"))

(check-report)
