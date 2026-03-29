(import (liii check)
        (liii string))

;; string-trim
;; 从字符串开头移除指定的字符/空白字符。
;;
;; 语法
;; ----
;; (string-trim str)
;; (string-trim str char)
;; (string-trim str pred?)
;; (string-trim str char/pred? start)
;; (string-trim str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：指定要从开头移除的字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值
;; - 省略时默认为字符空白字符空格(#\ )
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
;; string-trim会从字符串的左侧（开头）开始移除字符，直到遇到第一个不匹配指定条件的字符为止。
;; 当使用谓词参数时，所有使谓词返回#t的连续字符都会被移除。
;;
;; 对于空字符串，始终返回空字符串。
;; 当字符串以不匹配的字符开头，或字符串为空字符串时，返回原字符串的副本。
;;
;; 示例
;; ----
;; (string-trim "  hello  ") => "hello  "
;; (string-trim "---hello---" #\-) => "hello---"
;; (string-trim "   hello   ") => "hello   "
;; (string-trim "123hello123" char-numeric?) => "hello123"
;; (string-trim "hello") => "hello"
;; (string-trim "") => ""
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

(check (string-trim "  hello  ") => "hello  ")
(check (string-trim "---hello---" #\-) => "hello---")
(check (string-trim "123hello123" char-numeric?) => "hello123")
(check (string-trim "   ") => "")
(check (string-trim "") => "")
(check (string-trim "hello" #\-) => "hello")
(check (string-trim "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim "  hello  " #\space 2 7) => "hello")
(check (string-trim "   hello   " #\space 3) => "hello   ")
(check (string-trim "   hello   " #\space 3 8) => "hello")
(check (string-trim "---hello---" #\- 3 8) => "hello")
(check (string-trim "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim "123hello123" char-numeric? 3) => "hello123")

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
;; - 省略时默认为字符空白字符空格(#\ )
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

;; string-trim-both
;; 从字符串开头和末尾同时移除指定的字符/空白字符。
;;
;; 语法
;; ----
;; (string-trim-both str)
;; (string-trim-both str char)
;; (string-trim-both str pred?)
;; (string-trim-both str char/pred? start)
;; (string-trim-both str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：指定要从开头和末尾移除的字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值
;; - 省略时默认为字符空白字符空格(#\ )
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
;; 一个新的字符串，从开头和末尾同时移除所有连续的指定字符。
;;
;; 注意
;; ----
;; string-trim-both会同时从字符串的左侧（开头）和右侧（末尾）移除字符，是string-trim和string-trim-right的组合功能。
;;
;; 当使用谓词参数时，所有使谓词返回#t的连续字符都会被移除。
;;
;; 对于空字符串，始终返回空字符串。
;;
;; 示例
;; ----
;; (string-trim-both "  hello  ") => "hello"
;; (string-trim-both "---hello---" #\-) => "hello"
;; (string-trim-both "123hello123" char-numeric?) => "hello"
;; (string-trim-both "   ") => ""
;; (string-trim-both "hello") => "hello"
;; (string-trim-both "") => ""
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

(check (string-trim-both "  hello  ") => "hello")
(check (string-trim-both "---hello---" #\-) => "hello")
(check (string-trim-both "123hello123" char-numeric?) => "hello")
(check (string-trim-both "   ") => "")
(check (string-trim-both "") => "")
(check (string-trim-both "hello" #\-) => "hello")
(check (string-trim-both "abcABC123" char-upper-case?) => "abcABC123")
(check (string-trim-both "  hello  " #\space 2 7) => "hello")
(check (string-trim-both "   hello   " #\space 3) => "hello")
(check (string-trim-both "   hello   " #\space 3 8) => "hello")
(check (string-trim-both "---hello---" #\- 3 8) => "hello")
(check (string-trim-both "123hello123" char-numeric? 3 8) => "hello")
(check (string-trim-both "123hello123" char-numeric? 3) => "hello")

(check-report)
