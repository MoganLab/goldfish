;; string-index-of
;; 从 start 起查找子串 sub 的起始索引；找不到返回 #f。
;;
;; 语法
;; ----
;; (string-index-of s sub start)
;;
;; 参数
;; ----
;; s : string
;; 待搜索的字符串。
;; sub : string
;; 待匹配的子串。
;; start : integer
;; 起始搜索位置。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 匹配时返回起始索引，未匹配返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (string-index-of "hello world" "world" 0) => 6)
(check (string-index-of "hello world" "o" 0) => 4)
(check (string-index-of "hello world" "xyz" 0) => #f)
(check (string-index-of "aaa" "a" 1) => 1)
(check (string-index-of "hello" "h" 10) => #f)
(check (string-index-of "abc" "" 0) => 0)

(check-report)
