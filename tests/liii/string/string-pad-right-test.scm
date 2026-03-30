(import (liii check)
        (liii string)
) ;import

;; string-pad-right
;; 在字符串右侧填充字符以达到指定长度。
;;
;; 语法
;; ----
;; (string-pad-right str len)
;; (string-pad-right str len char)
;; (string-pad-right str len char start)
;; (string-pad-right str len char start end)
;;
;; 参数
;; ----
;; str : string?
;; 要填充的源字符串。
;;
;; len : integer?
;; 目标字符串长度，必须为非负整数。
;;
;; char : char? 可选
;; 要使用的填充字符，默认为空格字符(#\ )。
;;
;; start : integer? 可选
;; 子字符串起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 子字符串结束位置（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; string
;; 一个新的字符串。
;; - 当源字符串长度小于len时，在右侧添加指定填充字符以达到len长度。
;; - 当源字符串长度大于len时，返回左侧截取的len长度子串。
;; - 当源字符串长度等于len时，返回源字符串或其子串的副本。
;;
;; 注意
;; ----
;; string-pad-right是右填充(right padding)函数，填充字符添加在字符串后面。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。
;;
;; 示例
;; ----
;; (string-pad-right "abc" 6) => "abc   "
;; (string-pad-right "abc" 6 #\0) => "abc000"
;; (string-pad-right "abcdef" 3) => "abc"
;; (string-pad-right "" 5) => "     "
;; (string-pad-right "a" 1) => "a"
;;
;; 错误处理
;; ----
;; out-of-range 当len为负数时
;; wrong-type-arg 当str不是字符串类型时

; 基本功能测试
(check (string-pad-right "abc" 6) => "abc   ")
(check (string-pad-right "abc" 6 #\0) => "abc000")
(check (string-pad-right "abcdef" 3) => "abc")
(check (string-pad-right "abcdef" 3 #\0) => "abc")
(check (string-pad-right "" 5) => "     ")
(check (string-pad-right "" 5 #\0) => "00000")
(check (string-pad-right "a" 1) => "a")
(check (string-pad-right "abc" 3) => "abc")

; 边界情况测试
(check (string-pad-right "abc" 0) => "")
(check (string-pad-right "abc" 2) => "ab")
(check (string-pad-right "abc" 1) => "a")

; 多字节字符测试
(check (string-pad-right "中文" 6) => "中文")

; 子字符串范围参数测试
(check (string-pad-right "HelloWorld" 12 #\!) => "HelloWorld!!")
(check (string-pad-right "HelloWorld" 7 #\! 0 5) => "Hello!!")
(check (string-pad-right "HelloWorld" 8 #\! 1 6) => "elloW!!!")
(check (string-pad-right "HelloWorld" 5 #\x 3 5) => "loxxx")
(check (string-pad-right "HelloWorld" 0 #\! 3 3) => "")

; 多种填充字符测试
(check (string-pad-right "abc" 10 #\*) => "abc*******")
(check (string-pad-right "test" 8 #\-) => "test----")
(check (string-pad-right "123" 7 #\0) => "1230000")

; 错误处理测试
(check-catch 'out-of-range (string-pad-right "abc" -1))

(check-report)
