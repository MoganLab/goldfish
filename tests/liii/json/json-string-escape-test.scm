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

(check (json-string-escape "hello") => "\"hello\"")
(check (json-string-escape "hello\"world") => "\"hello\\\"world\"")
(check (json-string-escape "hello\\world") => "\"hello\\\\world\"")
(check (json-string-escape "hello/world") => "\"hello\\/world\"")
(check (json-string-escape "hello\bworld") => "\"hello\\bworld\"")
(check (json-string-escape "hello\fworld") => "\"hello\\fworld\"")
(check (json-string-escape "hello\nworld") => "\"hello\\nworld\"")
(check (json-string-escape "hello\rworld") => "\"hello\\rworld\"")
(check (json-string-escape "hello\tworld") => "\"hello\\tworld\"")
(check (json-string-escape "") => "\"\"")
(check (json-string-escape "A") => "\"A\"")
(check (json-string-escape "\"") => "\"\\\"\"")
(check (json-string-escape "\\") => "\"\\\\\"")
(check (json-string-escape "ABC") => "\"ABC\"")
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=")
       => "\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=\""
) ;check
(check (json-string-escape "SGVsbG8gV29ybGQ=") => "\"SGVsbG8gV29ybGQ=\"")
(check (json-string-escape "VGhpcyBpcyBhIHRlc3Q=") => "\"VGhpcyBpcyBhIHRlc3Q=\"")
(check (json-string-escape "QWxhZGRpbjpvcGVuIHNlc2FtZQ==") => "\"QWxhZGRpbjpvcGVuIHNlc2FtZQ==\"")

(let
  ((large-base64
     (string-append
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
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567")
     ) ;string-append
  ) ;
  (check (json-string-escape large-base64)
         => (string-append "\"" large-base64 "\"")
  ) ;check
) ;let

(check (json-string-escape "Hello123+=") => "\"Hello123+=\"")
(check (json-string-escape "Base64WithNewline\nText") => "\"Base64WithNewline\\nText\"")
(check (json-string-escape "Base64With\"Quote") => "\"Base64With\\\"Quote\"")

(let ((threshold-base64 (make-string 1000 #\A)))
  (check (json-string-escape threshold-base64)
         => (string-append "\"" threshold-base64 "\"")
  ) ;check
) ;let

(let ((large-base64-1001 (string-append (make-string 1001 #\A))))
  (check (json-string-escape large-base64-1001)
         => (string-append "\"" large-base64-1001 "\"")
  ) ;check
) ;let

(let ((mixed-large (string-append "Quote\"InFirst100" (make-string 990 #\A))))
  (check (json-string-escape mixed-large)
         => (string-append "\"Quote\\\"InFirst100" (make-string 990 #\A) "\"")
  ) ;check
) ;let

(check (json-string-escape "1234567890") => "\"1234567890\"")
(check (json-string-escape "0123456789ABCDEFabcdef") => "\"0123456789ABCDEFabcdef\"")
(check (json-string-escape "URLsafe_Base64chars") => "\"URLsafe_Base64chars\"")

(let ((long-escaped (make-string 50 #\")))
  (check (string-length (json-string-escape long-escaped)) => 102)
) ;let

(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZ") => "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"")
(check (json-string-escape "abcdefghijklmnopqrstuvwxyz") => "\"abcdefghijklmnopqrstuvwxyz\"")
(check (json-string-escape "0123456789") => "\"0123456789\"")
(check (json-string-escape "+=") => "\"+=\"")

(check-report)
