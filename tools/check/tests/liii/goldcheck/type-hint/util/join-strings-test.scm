;; join-strings
;; 使用指定分隔符连接字符串列表。
;;
;; 语法
;; ----
;; (join-strings lst sep)
;;
;; 参数
;; ----
;; lst : list of string  字符串列表
;; sep : string          分隔符字符串
;;
;; 返回值
;; ------
;; string
;; 拼接后的字符串。

(import (scheme base) (liii check) (liii goldcheck type-hint util))

(check-set-mode! 'report-failed)

;; 1. 多个字符串拼接
(check (join-strings '("a" "b" "c") ", ") => "a, b, c")

;; 2. 单元素列表
(check (join-strings '("solo") "-") => "solo")

;; 3. 空列表
(check (join-strings '() "/") => "")

(check-report)
