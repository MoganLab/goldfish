;; prefix-lines
;; 给每个逻辑行加前缀；空行只加前缀本身，不带尾随空格。
;;
;; 语法
;; ----
;; (prefix-lines prefix lines)
;;
;; 参数
;; ----
;; prefix : string
;; 前缀字符串，如 "> "。
;; lines : list
;; 逻辑行字符串列表。
;;
;; 返回值
;; ----
;; list
;; 加前缀后的行列表。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? prefix-lines))

(check (prefix-lines "> " '()) => '())
(check (prefix-lines "> " '("a")) => '("> a"))
(check (prefix-lines "> " '("a" "" "b")) => '("> a" ">" "> b"))
(check (prefix-lines "    " '("x" "")) => '("    x" ""))

(check-report)
