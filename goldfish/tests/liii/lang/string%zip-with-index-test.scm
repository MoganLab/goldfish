;; string%zip-with-index
;; 将字符串中每个字符与从 0 开始的下标配对（对标 Scala String.zipWithIndex）。
;;
;; 语法
;; ----
;; ($ s :zip-with-index)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; list?
;; ((char 0) (char 1) ...) 字符二元组链表，UTF-8 字符序；空串返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "ab" :zip-with-index) => '((#\a 0) (#\b 1)))
(check ($ "测试" :zip-with-index) => '((#\测 0) (#\试 1)))
(check ($ "" :zip-with-index) => '())

(check-report)
