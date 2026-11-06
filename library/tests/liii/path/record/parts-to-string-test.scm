;; parts->string
;; 将纯净的分量向量用分隔符拼接成字符串。
;;
;; 语法
;; ----
;; (parts->string parts sep)
;;
;; 参数
;; ----
;; parts : vector?
;; 分量向量（不含 drive/anchor）。
;; sep : string?
;; 连接分隔符，posix 传 "/"，Windows 传 "\\"。
;;
;; 返回值
;; ----
;; string?
;; 拼接后的字符串。

(import (liii check) (liii path record))

(check-set-mode! 'report-failed)

(check (parts->string #("a" "b" "c") "/") => "a/b/c")
(check (parts->string #("a" "b") "\\") => "a\\b")
(check (parts->string #("only") "/") => "only")
(check (parts->string #() "/") => "")

(check-report)
