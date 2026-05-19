(import (liii check) (liii string))

;; char-position
;; 在字符串中查找字符（或子串）首次出现的位置。
;;
;; 语法
;; ----
;; (char-position char-or-str str)
;; (char-position char-or-str str start)
;;
;; 参数
;; ----
;; char-or-str : char? 或 string?
;; 要查找的字符或子串。
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
;; 如果找到则返回位置索引，否则返回 #f。
;;
;; 注意
;; ----
;; 这是 s7 内置函数，(liii string) 重新导出以便统一使用。
;; 与 string-position 不同，char-position 支持按字符查找。

(check (char-position #\o "hello world") => 4)
(check (char-position #\z "hello world") => #f)
(check (char-position #\h "hello") => 0)
(check (char-position #\o "hello world" 5) => 7)
(check-true (integer? (char-position #\a "abc")))

(check-report)
