(import (liii check) (liii base64))
(check-set-mode! 'report-failed)

;; bytevector-base64-decode
;; 对 bytevector 的前 n 字节做 Base64 解码。
;;
;; 语法
;; ----
;; (bytevector-base64-decode bv)
;; (g_bytevector-base64-decode bv len)
;;
;; 参数
;; ----
;; bv : bytevector?
;; 要解码的字节向量。
;;
;; len : integer?
;; 参与解码的字节数，0 <= len <= (bytevector-length bv)，且必须是 4 的倍数。
;;
;; 返回值
;; -----
;; bytevector?
;; 解码结果。
;;
;; 说明
;; ----
;; 公开入口 bytevector-base64-decode 解码整个 bytevector；
;; C 层入口 g_bytevector-base64-decode 支持指定长度。

;; ; 基本功能测试
(check (g_bytevector-base64-decode (make-bytevector 8 65) 8)
  =>
  (make-bytevector 6 0)
) ;check
(check (string-base64-decode "QUFBQQ==") => "AAAA")

;; ; 长度参数测试
;; len 必须是 integer? 且 0 <= len <= (bytevector-length bv)，
;; 否则解码循环会按声明长度越界读取内存 (devel/0146.md)
(check-catch 'type-error
  (g_bytevector-base64-decode (make-bytevector 4 65) "4")
) ;check-catch
(check-catch 'value-error
  (g_bytevector-base64-decode (make-bytevector 4 65) -8)
) ;check-catch
(check-catch 'value-error (g_bytevector-base64-decode (make-bytevector 4 65) 8))
(check-catch 'value-error
  (g_bytevector-base64-decode (make-bytevector 4 65) 1073741824)
) ;check-catch

(check-report)
