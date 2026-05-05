(import (liii check) (liii ascii))


;; ascii-downcase
;; 将 ASCII 字母转换为小写。
;;
;; 语法
;; ----
;; (ascii-downcase x)
;;
;; 参数
;; ----
;; x : char? | integer? | string?
;; 要转换的字符、码点或字符串。
;;
;; 返回值
;; ----
;; char | integer | string
;; 返回与输入同类型的转换结果。字符串参数返回新的字符串。
;;
;; 注意
;; ----
;; 非字母输入会原样返回。
;; - 本函数是 `(scheme char)` 中 `char-downcase` 的 ASCII 特化版本。
;; - 对于 ASCII 字符，`ascii-downcase` 与 `char-downcase` 结果一致。
;; - `ascii-downcase` 同时支持字符、整数和字符串参数，而 `char-downcase` 仅接受字符。
;; - 字符串参数必须是纯 ASCII 字符串，否则会抛出 `value-error`。
;;
;; 相关函数
;; ----
;; (scheme char) 中的 string-downcase — 支持完整 Unicode 大小写转换的字符串转换函数。
;;   与 `ascii-downcase` 不同，`string-downcase` 可以处理非 ASCII 字符（如 `É` → `é`）。
;;
;; 示例
;; ----
;; (ascii-downcase #\A) => #\a
;; (ascii-downcase 65) => 97
;; (ascii-downcase "HELLO") => "hello"
;;
;; 错误处理
;; ------
;; value-error 当字符串参数包含非 ASCII 字符时


(check (ascii-downcase #\A) => #\a)
(check (ascii-downcase #\a) => #\a)
(check (ascii-downcase #\?) => #\?)
(check (ascii-downcase 65) => 97)
(check (ascii-downcase 97) => 97)
(check (ascii-downcase "HELLO") => "hello")
(check (ascii-downcase "hello") => "hello")
(check (ascii-downcase "Hello World 123") => "hello world 123")
(check (ascii-downcase "") => "")
(check-catch 'value-error (ascii-downcase "Héllo"))


(check-report)
