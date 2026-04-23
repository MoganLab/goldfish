(import (liii check) (liii string))

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
;; boolean
;; 如果str包含sub-str返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 推荐使用 string-contains?，其参数顺序和命名更符合直觉。
;; 查看 string-contains? 的完整文档：gf doc "string-contains?"
;; 空字符串作为sub-str时总是返回#t。

;;
;; 相关实现
;; --------
;; (liii string-cursor) 库中也提供了 string-contains 函数，
;; 该版本支持 Unicode 字符级别的操作，并提供 cursor-based API。
;; 参见: gf doc liii/string-cursor "string-contains"

(check-true (string-contains "0123456789" "3")
) ;check-true
(check-true (string-contains "0123456789" "34")
) ;check-true
(check-false (string-contains "0123456789" "24")
) ;check-false

(check-report)
