;; bigint->bytes
;; 将 bigint 转换为二进制补码字节向量。
;;
;; 语法
;; ----
;; (bigint->bytes n)
;; (bigint->bytes n big-endian?)
;;
;; 参数
;; ----
;; n : bigint?
;;   要编码的 bigint。
;; big-endian? : boolean?
;;   是否输出为大端序，默认 #t。
;;
;; 返回值
;; ----
;; bytevector?
;;   最简二进制补码表示。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->bytes (string->bigint "0")) => #u8(0))
(check (bigint->bytes (string->bigint "255")) => #u8(0 255))
(check (bigint->bytes (string->bigint "-1")) => #u8(255))
(check (bigint->bytes (string->bigint "-129")) => #u8(255 127))
(check (bigint->bytes (string->bigint "255") #f) => #u8(255 0))
(check (bigint->bytes (string->bigint "-1") #f) => #u8(255))

(check-catch 'type-error (bigint->bytes 123))

(check-report)
