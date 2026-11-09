;; string-trim-left
;; 从字符串开头移除指定的字符/空白字符。
;;
;; 语法
;; ----
;; (string-trim-left str)
;; (string-trim-left str char)
;; (string-trim-left str pred?)
;; (string-trim-left str char/pred? start)
;; (string-trim-left str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：指定要从开头移除的字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值
;; - 省略时默认为 ascii-whitespace?（仅移除 ASCII 空白字符）
;;
;; start : integer? 可选
;; 起始位置索引（包含），默认为0。
;;
;; end : integer? 可选
;; 结束位置索引（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; string
;; 一个新的字符串，从开头移除所有连续的指定字符。
;;
;; 注意
;; ----
;; string-trim-left 是 (liii string) 对 SRFI-13 string-trim 的别名，
;; 语义与 string-trim 完全一致，仅保留左侧修整这一惯用命名。
;;
;; 对于空字符串，始终返回空字符串；
;; 当字符串以不匹配的字符开头时，返回原字符串的副本。
;;
;; 错误处理
;; ----
;; type-error 当str不是字符串类型时
;; type-error 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

(import (liii check) (scheme char) (liii error) (liii string))


(check-set-mode! 'report-failed)


;; 默认移除 ASCII 空白字符
(check (string-trim-left "  hello  ") => "hello  ")
(check (string-trim-left "\t\nhello") => "hello")
(check (string-trim-left "hello") => "hello")
(check (string-trim-left "") => "")

;; 指定字符
(check (string-trim-left "xxabc" #\x) => "abc")
(check (string-trim-left "abcxx" #\x) => "abcxx")
(check (string-trim-left "xxx" #\x) => "")

;; 指定谓词
(check (string-trim-left "123abc" char-numeric?) => "abc")
(check (string-trim-left "abc123" char-numeric?) => "abc123")

;; 错误处理
(check-catch 'type-error (string-trim-left 123))
(check-catch 'type-error (string-trim-left "abc" "x"))
(check-catch 'out-of-range (string-trim-left "abc" #\a 0 10))


(check-report)
