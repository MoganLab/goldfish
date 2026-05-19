(import (liii check) (liii string))

;; string-position
;; 在字符串中查找子串的起始位置。
;;
;; 语法
;; ----
;; (string-position sub-str str)
;; (string-position sub-str str start)
;;
;; 参数
;; ----
;; sub-str : string?
;; 要查找的子串。
;;
;; str : string?
;; 被搜索的源字符串。
;;
;; start : integer? (可选，默认为 0)
;; 搜索的起始位置。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 如果找到子串，返回其起始位置索引；否则返回 #f。
;;
;; 注意
;; ----
;; 这是 s7 内置函数，(liii string) 重新导出以便统一使用。
;; 参数顺序为 (sub-str str)，与 srfi-13 的 string-contains 不同。
;; 空字符串作为 sub-str 时返回 #f。

(test (string-position "34" "0123456789") 3)
(test (string-position "012" "0123456789") 0)
(check-false (string-position "" "hello"))
(check-false (string-position "abc" "0123456789"))
(test (string-position "34" "0123434567" 4) 5)

(check-report)
