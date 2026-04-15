(import (liii base64) (liii check))

(check-set-mode! 'report-failed)

;; base64-decode
;; 将 Base64 格式的字符串解码为原始字符串。
;;
;; 语法
;; ----
;; (base64-decode str)
;;
;; 参数
;; ----
;; str : string
;; 要解码的 Base64 字符串。
;;
;; 返回值
;; ----
;; string
;; 解码后的原始字符串。
;;
;; 注意
;; ----
;; - 空字符串解码为空字符串
;; - 支持带'='填充的 Base64 字符串
;;
;; 错误处理
;; ----
;; 无

(check (base64-decode "") => "")

(check (base64-decode "YQ==") => "a")
(check (base64-decode "eg==") => "z")
(check (base64-decode "Zg==") => "f")
(check (base64-decode "Zm8=") => "fo")
(check (base64-decode "Zm9v") => "foo")
(check (base64-decode "Zm9vYg==")
  =>
  "foob"
) ;check
(check (base64-decode "Zm9vYmE=")
  =>
  "fooba"
) ;check
(check (base64-decode "Zm9vYmFy")
  =>
  "foobar"
) ;check

(check-report)
