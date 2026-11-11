;; token-bytes
;; 生成指定长度的密码学安全随机字节向量。
;;
;; 语法
;; ----
;; (token-bytes [nbytes])
;;
;; 参数
;; ----
;; nbytes : exact-integer?
;; 随机字节数，默认 32；必须是非负精确整数。
;;
;; 返回值
;; -----
;; bytevector
;; 长度为 nbytes 的随机字节向量。
;;
;; 说明
;; ----
;; 1. 随机性来自操作系统熵源，不可预测。
;; 2. 默认 32 字节（256 位）满足常见安全凭据的强度要求。
;;
;; 错误处理
;; ----
;; nbytes 不是精确整数时抛出 type-error；nbytes 为负数时抛出 value-error。

(import (liii check) (liii secrets))

(check-set-mode! 'report-failed)

;; 默认生成 32 字节
(check (bytevector? (token-bytes)) => #t)
(check (bytevector-length (token-bytes)) => 32)

;; 显式指定长度
(check (bytevector-length (token-bytes 0)) => 0)
(check (bytevector-length (token-bytes 1)) => 1)
(check (bytevector-length (token-bytes 16)) => 16)
(check (bytevector-length (token-bytes 64)) => 64)

;; 0 字节返回空 bytevector
(check (token-bytes 0) => #u8())

;; 每个字节都是 0..255 的无符号整数
(let ((bv (token-bytes 32)))
  (let loop
    ((i 0))
    (if (>= i (bytevector-length bv))
      #t
      (let ((b (bytevector-u8-ref bv i)))
        (check (exact-integer? b) => #t)
        (check (>= b 0) => #t)
        (check (<= b 255) => #t)
        (loop (+ i 1))
      ) ;let
    ) ;if
  ) ;let
) ;let

;; 多次生成结果互不相同
(let ((a (token-bytes 32)) (b (token-bytes 32)))
  (check (equal? a b) => #f)
) ;let

;; 参数个数错误
(check-catch 'wrong-number-of-args (token-bytes 1 2))

;; 非法参数
(check-catch 'type-error (token-bytes 3.14))
(check-catch 'type-error (token-bytes "16"))
(check-catch 'type-error (token-bytes #t))
(check-catch 'value-error (token-bytes -1))

(check-report)
