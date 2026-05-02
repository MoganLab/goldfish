(import (liii check) (liii ascii))


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
;; - 本函数是 `(scheme char)` 中 `char-upcase` 的 ASCII 特化版本。
;; - 对于 ASCII 字符，`ascii-upcase` 与 `char-upcase` 结果一致。
;; - `ascii-upcase` 同时支持字符和整数参数，而 `char-upcase` 仅接受字符。
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
