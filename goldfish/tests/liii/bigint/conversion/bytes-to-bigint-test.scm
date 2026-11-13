;; bytes->bigint
;; 将二进制补码字节向量转换为 bigint。
;;
;; 语法
;; ----
;; (bytes->bigint bytes)
;; (bytes->bigint bytes big-endian?)
;;
;; 参数
;; ----
;; bytes : bytevector?
;;   包含二进制补码表示的字节向量。
;; big-endian? : boolean?
;;   是否按大端序解释，默认 #t。
;;
;; 返回值
;; ----
;; bigint?
;;   解析得到的 bigint。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是字节向量时抛出。

(import (liii check)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (bytes->bigint #u8(0))) => "0")
(check (bigint->string (bytes->bigint #u8(0 255))) => "255")
(check (bigint->string (bytes->bigint #u8(255))) => "-1")
(check (bigint->string (bytes->bigint #u8(255 127))) => "-129")
(check (bigint->string (bytes->bigint #u8(255 0) #f)) => "255")
(check (bigint->string (bytes->bigint #u8(255 255) #f)) => "-1")

(check-catch 'type-error (bytes->bigint 123))

(check-report)
