(import (liii check) (liii json) (liii base) (liii error) (liii unicode))

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
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
       ) ;json-string-escape
  =>
  "\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=\""
) ;check
(check (json-string-escape "SGVsbG8gV29ybGQ=") => "\"SGVsbG8gV29ybGQ=\"")
(check (json-string-escape "VGhpcyBpcyBhIHRlc3Q=")
  =>
  "\"VGhpcyBpcyBhIHRlc3Q=\""
) ;check
(check (json-string-escape "QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
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

(check (json-string-escape "Hello123+=") => "\"Hello123+=\"")
(check (json-string-escape "Base64WithNewline\nText")
  =>
  "\"Base64WithNewline\\nText\""
) ;check
(check (json-string-escape "Base64With\"Quote") => "\"Base64With\\\"Quote\"")

(let ((threshold-base64 (make-string 1000 #\A)))
  (check (json-string-escape threshold-base64)
    =>
    (string-append "\"" threshold-base64 "\"")
  ) ;check
) ;let

(let ((large-base64-1001 (string-append (make-string 1001 #\A))))
  (check (json-string-escape large-base64-1001)
    =>
    (string-append "\"" large-base64-1001 "\"")
  ) ;check
) ;let

(let ((mixed-large (string-append "Quote\"InFirst100" (make-string 990 #\A))))
  (check (json-string-escape mixed-large)
    =>
    (string-append "\"Quote\\\"InFirst100" (make-string 990 #\A) "\"")
  ) ;check
) ;let

;; 多字节 UTF-8 字符：直接输出原始字符，不做转义
(check (json-string-escape "你好") => "\"你好\"")
(check (json-string-escape "中文/English") => "\"中文\\/English\"")
(check (json-string-escape "你好\n世界") => "\"你好\\n世界\"")
(check (json-string-escape "é") => "\"é\"")
(check (json-string-escape "😀") => "\"😀\"")
(check (json-string-escape "日\"本\"語") => "\"日\\\"本\\\"語\"")
(check (json-string-escape "aé 😀 你好\\mix")
  =>
  "\"aé 😀 你好\\\\mix\""
) ;check
(let ((long-utf8 (utf8-make-string 500 #\中)))
  (check (json-string-escape long-utf8) => (string-append "\"" long-utf8 "\""))
) ;let

;; 其他控制字符（< 0x20）转义为 \uXXXX，保证输出是可再解析的 JSON
(check (json-string-escape (string #\x1)) => "\"\\u0001\"")
(check (json-string-escape (string #\x1f)) => "\"\\u001f\"")
(check (json-string-escape (string #\a #\null #\b)) => "\"a\\u0000b\"")

(check (json-string-escape "1234567890") => "\"1234567890\"")
(check (json-string-escape "0123456789ABCDEFabcdef")
  =>
  "\"0123456789ABCDEFabcdef\""
) ;check
(check (json-string-escape "URLsafe_Base64chars") => "\"URLsafe_Base64chars\"")

(let ((long-escaped (make-string 50 #\")))
  (check (string-length (json-string-escape long-escaped)) => 102)
) ;let
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  =>
  "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\""
) ;check
(check (json-string-escape "abcdefghijklmnopqrstuvwxyz")
  =>
  "\"abcdefghijklmnopqrstuvwxyz\""
) ;check
(check (json-string-escape "0123456789") => "\"0123456789\"")
(check (json-string-escape "+=") => "\"+=\"")

(check-report)
