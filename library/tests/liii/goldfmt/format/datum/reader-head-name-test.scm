;; reader-head-name
;; 读取表单头符号的名称。
;;
;; 语法
;; ----
;; (reader-head-name head)
;;
;; 参数
;; ----
;; head : any?
;; 表单的第一个元素。
;;
;; 返回值
;; ----
;; string?
;; 头符号的名称，非符号时返回 "default"。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-head-name 'car) => "car")
(check (reader-head-name 42) => "default")

(check-report)
