(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-replace
;; 替换字符串中的指定子串。
;;
;; 语法
;; ----
;; (string-replace s1 s2 start1 end1 [start2 end2])
;;
;; 参数
;; ----
;; s1 : string
;; 被替换的字符串
;;
;; s2 : string
;; 用于替换的字符串
;;
;; start1 : integer
;; s1 中被替换子串的起始位置
;;
;; end1 : integer
;; s1 中被替换子串的结束位置
;;
;; start2 : integer (可选)
;; s2 中用于替换的子串起始位置，默认为0
;;
;; end2 : integer (可选)
;; s2 中用于替换的子串结束位置，默认为 s2 字符数
;;
;; 返回值
;; ------
;; string?
;; 替换后的新字符串
;;
;; 说明
;; ----
;; 1. 返回新字符串，不修改原字符串
;; 2. 与 (liii string) 的区别：(liii string-cursor) 按字符位置操作，支持 Unicode

(check (string-replace "It's easy to code it up in Scheme." "lots of fun" 5 9) => "It's lots of fun to code it up in Scheme.")
(check (string-replace "abcdef" "xyz" 2 4) => "abxyzef")

;; Unicode 测试
(check (string-replace "中文测试" "示例" 2 4) => "中文示例")

(check-report)
