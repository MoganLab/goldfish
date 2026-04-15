(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-string-escape
;; 将字符串转为 JSON 字符串字面量。
;;
;; 语法
;; ----
;; (json-string-escape string)
;;
;; 参数
;; ----
;; string : string?
;; 要转义的原始字符串。
;;
;; 返回值
;; ----
;; string
;; 返回已加双引号并完成 JSON 转义的字符串。
;;
;; 注意
;; ----
;; 对于较长且安全的 Base64 风格字符串，会走快速路径优化。
;;
;; 示例
;; ----
;; (json-string-escape "hello") => "\"hello\""
;; (json-string-escape "hello\\world") => "\"hello\\\\world\""
;;
;; 错误处理
;; ----
;; 无。

(check (json-string-escape "hello")
  =>
  "\"hello\""
) ;check
(check (json-string-escape "hello\"world")
  =>
  "\"hello\\\"world\""
) ;check
(check (json-string-escape "hello\\world")
  =>
  "\"hello\\\\world\""
) ;check
(check (json-string-escape "hello/world")
  =>
  "\"hello\\/world\""
) ;check
(check (json-string-escape "hello\bworld")
  =>
  "\"hello\\bworld\""
) ;check
(check (json-string-escape "hello\fworld")
  =>
  "\"hello\\fworld\""
) ;check
(check (json-string-escape "hello\nworld")
  =>
  "\"hello\\nworld\""
) ;check
(check (json-string-escape "hello\rworld")
  =>
  "\"hello\\rworld\""
) ;check
(check (json-string-escape "hello\tworld")
  =>
  "\"hello\\tworld\""
) ;check
(check (json-string-escape "")
  =>
  "\"\""
) ;check
(check (json-string-escape "A")
  =>
  "\"A\""
) ;check
(check (json-string-escape "\"")
  =>
  "\"\\\"\""
) ;check
(check (json-string-escape "\\")
  =>
  "\"\\\\\""
) ;check
(check (json-string-escape "ABC")
  =>
  "\"ABC\""
) ;check
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
       ) ;json-string-escape
  =>
  "\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=\""
) ;check
(check (json-string-escape "SGVsbG8gV29ybGQ=")
  =>
  "\"SGVsbG8gV29ybGQ=\""
) ;check
(check (json-string-escape "VGhpcyBpcyBhIHRlc3Q="
       ) ;json-string-escape
  =>
  "\"VGhpcyBpcyBhIHRlc3Q=\""
) ;check
(check (json-string-escape "QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
       ) ;json-string-escape
  =>
  "\"QWxhZGRpbjpvcGVuIHNlc2FtZQ==\""
) ;check

(let ((large-base64 (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567"
                    ) ;string-append
      ) ;large-base64
     ) ;
  (check (json-string-escape large-base64)
    =>
    (string-append "\"" large-base64 "\"")
  ) ;check
) ;let

(check (json-string-escape "Hello123+=")
  =>
  "\"Hello123+=\""
) ;check
(check (json-string-escape "Base64WithNewline\nText"
       ) ;json-string-escape
  =>
  "\"Base64WithNewline\\nText\""
) ;check
(check (json-string-escape "Base64With\"Quote")
  =>
  "\"Base64With\\\"Quote\""
) ;check

(let ((threshold-base64 (make-string 1000 #\A)
      ) ;threshold-base64
     ) ;
  (check (json-string-escape threshold-base64)
    =>
    (string-append "\""
      threshold-base64
      "\""
    ) ;string-append
  ) ;check
) ;let

(let ((large-base64-1001 (string-append (make-string 1001 #\A))
      ) ;large-base64-1001
     ) ;
  (check (json-string-escape large-base64-1001)
    =>
    (string-append "\""
      large-base64-1001
      "\""
    ) ;string-append
  ) ;check
) ;let

(let ((mixed-large (string-append "Quote\"InFirst100"
                     (make-string 990 #\A)
                   ) ;string-append
      ) ;mixed-large
     ) ;
  (check (json-string-escape mixed-large)
    =>
    (string-append "\"Quote\\\"InFirst100"
      (make-string 990 #\A)
      "\""
    ) ;string-append
  ) ;check
) ;let

(check (json-string-escape "1234567890")
  =>
  "\"1234567890\""
) ;check
(check (json-string-escape "0123456789ABCDEFabcdef"
       ) ;json-string-escape
  =>
  "\"0123456789ABCDEFabcdef\""
) ;check
(check (json-string-escape "URLsafe_Base64chars"
       ) ;json-string-escape
  =>
  "\"URLsafe_Base64chars\""
) ;check

(let ((long-escaped (make-string 50 #\")))
  (check (string-length (json-string-escape long-escaped)
         ) ;string-length
    =>
    102
  ) ;check
) ;let
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       ) ;json-string-escape
  =>
  "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\""
) ;check
(check (json-string-escape "abcdefghijklmnopqrstuvwxyz"
       ) ;json-string-escape
  =>
  "\"abcdefghijklmnopqrstuvwxyz\""
) ;check
(check (json-string-escape "0123456789")
  =>
  "\"0123456789\""
) ;check
(check (json-string-escape "+=")
  =>
  "\"+=\""
) ;check

(check-report)
