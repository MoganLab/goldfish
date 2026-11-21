;; string
;; 从给定字符构造字符串。
;;
;;
;; 语法
;; ----
;; (string char ...)
;;
;; 参数
;; ----
;; char ... : char?
;; 任意数量的字符。
;;
;; 返回值
;; ----
;; string?
;; 由给定字符组成的新字符串。
;;
;; 说明
;; ----
;; 1. 无参数时返回空字符串
;; 2. 参数顺序决定结果字符串中的字符顺序
;; 3. Unicode 字符按 UTF-8 编码构造，结果字符串字节长度为各字符 UTF-8 字节数之和

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (string) => "")
(check (string #\a) => "a")
(check (string #\a #\b #\c) => "abc")
(check (string #\1 #\2 #\3) => "123")
(check (string-ref (string #\xFF) 0) => #\xFF)
(check (string-length (string #\x #\y)) => 2)
;; string 支持 Unicode 字符（UTF-8 编码）
(check (string #\x100) => (string #\x100))
(check (string #\中) => "中")
(check (string #\a #\中) => "a中")
(check-catch 'type-error (string 'a))
(check-catch 'type-error (string 1))
(check-catch 'type-error (string "a"))

(check-report)
