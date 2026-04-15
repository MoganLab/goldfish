(import (liii check) (liii string))

;; string-upcase
;; 将字符串中的所有小写字母转化为大写字母。
;;
;; 语法
;; ----
;; (string-upcase str)
;; (string-upcase str start)
;; (string-upcase str start end)
;;
;; 参数
;; ----
;; str : string?
;; 要转换的字符串
;;
;; start : integer? 可选
;; transformation的起始位置(包含)，默认为0
;;
;; end : integer? 可选
;; transformation的结束位置(不包含)，默认为字符串长度
;;
;; 返回值
;; ----
;; string
;; 返回将str中指定范围内的小写字母转化为大写字母后的新字符串。
;;
;; 注意
;; ----
;; 仅在ASCII范围内进行大小写转换，非字母字符保持不变。
;; 当前实现对Unicode字符的支持有限。
;; 空字符串会返回空字符串。
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出字符串索引范围时

(check (string-upcase "abc") => "ABC")
(check (string-upcase "ABC") => "ABC")
(check (string-upcase "aBc") => "ABC")
(check (string-upcase "123") => "123")
(check (string-upcase "!@#") => "!@#")
(check (string-upcase "abc123xyz")
  =>
  "ABC123XYZ"
) ;check
(check (string-upcase "") => "")
(check (string-upcase "中文english123")
  =>
  "中文ENGLISH123"
) ;check
(check (string-upcase "mixedUPPERlower123")
  =>
  "MIXEDUPPERLOWER123"
) ;check

(check (string-upcase (make-string 0))
  =>
  ""
) ;check
(check (string-upcase (make-string 10 #\a))
  =>
  "AAAAAAAAAA"
) ;check

(check (string-upcase "abcdef" 0 1)
  =>
  "Abcdef"
) ;check
(check (string-upcase "abcdef" 0 3)
  =>
  "ABCdef"
) ;check
(check (string-upcase "abcdef" 2 4)
  =>
  "abCDef"
) ;check
(check (string-upcase "abcdef"
         3
         (string-length "abcdef")
       ) ;string-upcase
  =>
  "abcDEF"
) ;check
(check (string-upcase "abcdef"
         0
         (string-length "abcdef")
       ) ;string-upcase
  =>
  "ABCDEF"
) ;check
(check (string-upcase "abc" 0) => "ABC")
(check (string-upcase "abc" 1) => "aBC")

(check (string-upcase "space char space")
  =>
  "SPACE CHAR SPACE"
) ;check
(check (string-upcase "tab\tnewline\nreturn\r")
  =>
  "TAB\tNEWLINE\nRETURN\r"
) ;check

(check-catch 'out-of-range
  (string-upcase "abc" 0 4)
) ;check-catch
(check-catch 'out-of-range
  (string-upcase "abc" -1 2)
) ;check-catch
(check-catch 'out-of-range
  (string-upcase "abc" 2 1)
) ;check-catch

(check-report)
