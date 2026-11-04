;; reader-vector-prefix
;; 读取向量字面量的前缀。
;;
;; 语法
;; ----
;; (reader-vector-prefix datum)
;;
;; 参数
;; ----
;; datum : any?
;; 向量或字节向量。
;;
;; 返回值
;; ----
;; string?
;; "#(" 或 "#u8("。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-vector-prefix #(1)) => "#(")
(check (reader-vector-prefix (make-byte-vector 2 0)) => "#u8(")

(check-report)
