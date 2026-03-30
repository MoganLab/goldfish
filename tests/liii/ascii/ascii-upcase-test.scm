(import (liii check)
        (liii ascii))

;; ascii-upcase
;; 将 ASCII 字母转换为大写。
;;
;; 语法
;; ----
;; (ascii-upcase x)
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
;; (ascii-upcase #\a) => #\A
;; (ascii-upcase 97) => 65
;;
;; 错误处理
;; ----
;; 不需要转换时返回原值

(check (ascii-upcase #\a) => #\A)
(check (ascii-upcase #\A) => #\A)
(check (ascii-upcase #\?) => #\?)
(check (ascii-upcase 97) => 65)

(check-report)
