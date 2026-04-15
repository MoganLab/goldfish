(import (liii check) (liii string))

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

(check (string-pad-right "abc" 6)
  =>
  "abc   "
) ;check
(check (string-pad-right "abc" 6 #\0)
  =>
  "abc000"
) ;check
(check (string-pad-right "abcdef" 3)
  =>
  "abc"
) ;check
(check (string-pad-right "abcdef" 3 #\0)
  =>
  "abc"
) ;check
(check (string-pad-right "" 5)
  =>
  "     "
) ;check
(check (string-pad-right "" 5 #\0)
  =>
  "00000"
) ;check
(check (string-pad-right "a" 1) => "a")
(check (string-pad-right "abc" 3)
  =>
  "abc"
) ;check

(check (string-pad-right "abc" 0) => "")
(check (string-pad-right "abc" 2)
  =>
  "ab"
) ;check
(check (string-pad-right "abc" 1)
  =>
  "a"
) ;check

(check (string-pad-right "中文" 6)
  =>
  "中文"
) ;check

(check (string-pad-right "HelloWorld" 12 #\!)
  =>
  "HelloWorld!!"
) ;check
(check (string-pad-right "HelloWorld"
         7
         #\!
         0
         5
       ) ;string-pad-right
  =>
  "Hello!!"
) ;check
(check (string-pad-right "HelloWorld"
         8
         #\!
         1
         6
       ) ;string-pad-right
  =>
  "elloW!!!"
) ;check
(check (string-pad-right "HelloWorld"
         5
         #\x
         3
         5
       ) ;string-pad-right
  =>
  "loxxx"
) ;check
(check (string-pad-right "HelloWorld"
         0
         #\!
         3
         3
       ) ;string-pad-right
  =>
  ""
) ;check

(check (string-pad-right "abc" 10 #\*)
  =>
  "abc*******"
) ;check
(check (string-pad-right "test" 8 #\-)
  =>
  "test----"
) ;check
(check (string-pad-right "123" 7 #\0)
  =>
  "1230000"
) ;check

(check-catch 'out-of-range
  (string-pad-right "abc" -1)
) ;check-catch

(check-report)
