(import (liii check) (scheme char) (liii string))

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
;; 说明
;; ----
;; string-trim-left 是 SRFI-13 string-trim 的别名：SRFI-13 的 string-trim
;; 从字符串左侧（开头）开始移除字符，与 string-trim-left 语义一致。
;; 与 string-trim-right（移除尾部字符）、string-trim-both（两端都移除）相对。
;;
;; 对于空字符串，始终返回空字符串。
;; 当字符串以不匹配的字符开头，或字符串为空字符串时，返回原字符串的副本。
;;
;; Unicode 警告
;; --------
;; string-trim, string-trim-left, string-trim-right, string-trim-both
;; 这些函数在处理 Unicode 字符(如中文)时可能有字节截断问题。
;; 对于 Unicode 字符串，建议使用 (liii unicode) 中的 utf8-string-trim-* 系列函数。
;;
;; 示例
;; ----
;; (string-trim-left "  hello  ") => "hello  "
;; (string-trim-left "---hello---" #\-) => "hello---"
;; (string-trim-left "   hello   ") => "hello   "
;; (string-trim-left "123hello123" char-numeric?) => "hello123"
;; (string-trim-left "hello") => "hello"
;; (string-trim-left "") => ""
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

(check (string-trim-left "  hello  ") => "hello  ")
(check (string-trim-left "---hello---" #\-) => "hello---")
(check (string-trim-left "123hello123" char-numeric?) => "hello123")
(check (string-trim-left "   ") => "")
(check (string-trim-left "") => "")
(check (string-trim-left "hello" #\-) => "hello")
(check (string-trim-left "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim-left "  hello  " #\space 2 7) => "hello")
(check (string-trim-left "   hello   " #\space 3) => "hello   ")
(check (string-trim-left "   hello   " #\space 3 8) => "hello")
(check (string-trim-left "---hello---" #\- 3 8) => "hello")
(check (string-trim-left "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim-left "123hello123" char-numeric? 3) => "hello123")
(check (string-trim-left "hello   ") => "hello   ")
(check (string-trim-left "  hello") => "hello")

;; 与 string-trim 别名一致性
(check (string-trim-left "  hi  ") => (string-trim "  hi  "))

;; Unicode 行为与 string-trim 一致
(check (string-trim-left "中文") => "中文")
(check (string-trim-left " 中文 ") => "中文 ")
(check (string-trim-left "　中文") => "　中文")

;; ascii-whitespace? 默认不会移除非 ASCII 空白字节（如 160）
(let ((s1 (string (integer->char 160) #\h #\e #\l #\l #\o)))
  (check (string-trim-left s1) => s1)
) ;let

(check-report)
