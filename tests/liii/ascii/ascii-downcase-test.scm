(import (liii check)
        (liii ascii))

;; ascii-downcase
;; 将 ASCII 字母转换为小写。
;;
;; 语法
;; ----
;; (ascii-downcase x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要转换的字符或码点。
;;
;; 返回值
;; ----
;; char | integer
;; 返回与输入同类型的转换结果。
;;
;; 注意
;; ----
;; 非字母输入会原样返回。
;;
;; 示例
;; ----
;; (ascii-downcase #\A) => #\a
;; (ascii-downcase 65) => 97
;;
;; 错误处理
;; ----
;; 不需要转换时返回原值

(check (ascii-downcase #\A) => #\a)
(check (ascii-downcase #\a) => #\a)
(check (ascii-downcase #\?) => #\?)
(check (ascii-downcase 65) => 97)

(check-report)
