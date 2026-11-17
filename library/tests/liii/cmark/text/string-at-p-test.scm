;; string-at?
;; 判断字符串 s 从位置 pos 起是否匹配子串 sub（越界返回 #f）。
;;
;; 语法
;; ----
;; (string-at? s pos sub)
;;
;; 参数
;; ----
;; s : string
;; 目标字符串。
;; pos : integer
;; 起始位置索引。
;; sub : string
;; 待匹配子串。
;;
;; 返回值
;; ----
;; boolean
;; 合法位置且匹配成功返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (string-at? "hello" 0 "he") => #t)
(check (string-at? "hello" 2 "llo") => #t)
(check (string-at? "hello" 4 "llo") => #f)
(check (string-at? "hello" -1 "h") => #f)
(check (string-at? "hello" 0 "xx") => #f)
(check (string-at? "hello" 3 "") => #t)

(check-report)
