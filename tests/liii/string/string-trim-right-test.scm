(import (liii check) (scheme char) (liii string))

;; string-trim-right
;; 从字符串末尾移除指定的字符/空白字符。
;;
;; 语法
;; ----
;; (string-trim-right str)
;; (string-trim-right str char)
;; (string-trim-right str pred?)
;; (string-trim-right str char/pred? start)
;; (string-trim-right str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：指定要从末尾移除的字符
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
;; 一个新的字符串，从末尾移除所有连续的指定字符。
;;
;; 注意
;; ----
;; string-trim-right会从字符串的右侧（末尾）开始移除字符，直到遇到第一个不匹配指定条件的字符为止。
;; 当使用谓词参数时，所有使谓词返回#t的连续字符都会被移除。
;;
;; 对于空字符串，始终返回空字符串。
;; 当字符串以不匹配的字符结尾，或字符串为空字符串时，返回原字符串的副本。
;;
;; 示例
;; ----
;; (string-trim-right "  hello  ") => "  hello"
;; (string-trim-right "---hello---" #\-) => "---hello"
;; (string-trim-right "123hello123" char-numeric?) => "123hello"
;; (string-trim-right "   ") => ""
;; (string-trim-right "hello") => "hello"
;; (string-trim-right "") => ""
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

;;
;; 相关实现
;; --------
;; (liii string-cursor) 库中也提供了 string-trim-right 函数，
;; 该版本支持 Unicode 字符级别的操作，并提供 cursor-based API。
;; 参见: gf doc liii/string-cursor "string-trim-right"

(check (string-trim-right "  hello  ") => "  hello")
(check (string-trim-right "---hello---" #\-) => "---hello")
(check (string-trim-right "123hello123" char-numeric?) => "123hello")
(check (string-trim-right "   ") => "")
(check (string-trim-right "") => "")
(check (string-trim-right "hello" #\-) => "hello")
(check (string-trim-right "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim-right "  hello  " #\space 2 7) => "hello")
(check (string-trim-right "   hello   " #\space 3) => "hello")
(check (string-trim-right "   hello   " #\space 3 8) => "hello")
(check (string-trim-right "---hello---" #\- 3 8) => "hello")
(check (string-trim-right "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim-right "123hello123" char-numeric? 3) => "hello")

;; ascii-whitespace? 默认不会移除非 ASCII 空白字节（如 160）
(let ((s (string #\h #\e #\l #\l #\o (integer->char 160))))
  (check (string-trim-right s) => s)
) ;let

(check-report)
