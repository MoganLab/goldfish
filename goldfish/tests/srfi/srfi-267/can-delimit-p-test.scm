;; can-delimit?
;; 判断某个 delimiter 是否可用于表示给定字符串。
;;
;; 语法
;; ----
;; (can-delimit? str delimiter)
;;
;; 参数
;; ----
;; str : string?
;; 待表示的字符串内容。
;; 返回值
;; ----
;; boolean
;; delimiter 可用于表示 str 时返回 #t，否则返回 #f。
;; 说明
;; ----
;; delimiter 不能包含双引号，
;; 且字符串中不能出现 "<delimiter>" 这样的完整片段，也不能以 "<delimiter> 结尾。
;; 错误处理
;; ----
;; type-error 当任一参数不是字符串时抛出。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(check (can-delimit? "abc" "") => #t)
(check (can-delimit? "\"" "") => #f)
(check (can-delimit? "\"\"" "") => #f)
(check (can-delimit? "\"\"" "-") => #t)
(check (can-delimit? "abc" "\"") => #f)
(check (can-delimit? "abc" "tag with space") => #t)
(check (can-delimit? "abc" "(())") => #t)

;; 字符串以 "<delimiter> 结尾时不可用
(check (can-delimit? "abc\"-" "-") => #f)

;; 错误处理
(check-catch 'type-error (can-delimit? 1 ""))
(check-catch 'type-error (can-delimit? "abc" 1))

(check-report)
