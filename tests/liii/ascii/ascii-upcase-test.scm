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
;; - 本函数是 `(scheme char)` 中 `char-upcase` 的 ASCII 特化版本。
;; - 对于 ASCII 字符，`ascii-upcase` 与 `char-upcase` 结果一致。
;; - `ascii-upcase` 同时支持字符、整数和字符串参数，而 `char-upcase` 仅接受字符。
;; - 字符串参数必须是纯 ASCII 字符串，否则会抛出 `value-error`。
;;
;; 相关函数
;; ----
;; (scheme char) 中的 string-upcase — 支持完整 Unicode 大小写转换的字符串转换函数。
;;   与 `ascii-upcase` 不同，`string-upcase` 可以处理非 ASCII 字符（如 `é` → `É`）。
;;
;; 示例
;; ----
;; (ascii-upcase #\a) => #\A
;; (ascii-upcase 97) => 65
;; (ascii-upcase "hello") => "HELLO"
;;
;; 错误处理
;; ------
;; value-error 当字符串参数包含非 ASCII 字符时


(check (ascii-upcase #\a) => #\A)
(check (ascii-upcase #\A) => #\A)
(check (ascii-upcase #\?) => #\?)
(check (ascii-upcase 97) => 65)
(check (ascii-upcase 65) => 65)
(check (ascii-upcase "hello") => "HELLO")
(check (ascii-upcase "HELLO") => "HELLO")
(check (ascii-upcase "Hello World 123") => "HELLO WORLD 123")
(check (ascii-upcase "") => "")
(check-catch 'value-error (ascii-upcase "héllo"))


(check-report)
