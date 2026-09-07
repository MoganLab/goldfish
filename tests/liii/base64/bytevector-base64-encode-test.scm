(import (liii check) (liii base64))
(check-set-mode! 'report-failed)

;; bytevector-base64-encode
;; 对 bytevector 的前 n 字节做 Base64 编码。
;;
;; 语法
;; ----
;; (bytevector-base64-encode bv)
;; (g_bytevector-base64-encode bv len)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 要编码的字节向量。
;;
;; len : integer?
;; 参与编码的字节数，0 <= len <= (bytevector-length bv)。
;;
;; 返回值
;; -----
;; bytevector?
;; 编码结果。
;;
;; 说明
;; ----
;; 公开入口 bytevector-base64-encode 编码整个 bytevector；
;; C 层入口 g_bytevector-base64-encode 支持指定长度。

;; ; 基本功能测试
(check (string-base64-encode "AAAA") => "QUFBQQ==")
(check (utf8->string (g_bytevector-base64-encode (make-bytevector 4 65) 4))
  =>
  "QUFBQQ=="
) ;check
(check (utf8->string (g_bytevector-base64-encode (make-bytevector 4 65) 0))
  =>
  ""
) ;check

;; ; 长度参数测试
;; len 必须是 integer? 且 0 <= len <= (bytevector-length bv)，
;; 否则编码循环会按声明长度越界读取内存 (devel/0146.md)
(check-catch 'type-error
  (g_bytevector-base64-encode (make-bytevector 4 65) "4")
) ;check-catch
(check-catch 'value-error
  (g_bytevector-base64-encode (make-bytevector 4 65) -4)
) ;check-catch
(check-catch 'value-error
  (g_bytevector-base64-encode (make-bytevector 4 65) 1073741824)
) ;check-catch

(check-report)
