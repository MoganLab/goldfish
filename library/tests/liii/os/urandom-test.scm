;; urandom
;; 生成指定字节长度的密码学安全随机数据（CSPRNG）。
;;
;; 语法
;; ----
;; (urandom n)
;;
;; 参数
;; ----
;; n : exact-integer?
;; 随机字节数，必须是非负精确整数。
;;
;; 返回值
;; -----
;; bytevector
;; 长度为 n 的随机字节向量，每个字节取值范围为 0..255。
;;
;; 说明
;; ----
;; 1. 底层对接操作系统熵源：Linux 使用 getrandom(2)，macOS 使用
;;    getentropy(2)，Windows 使用 CNG 的 BCryptGenRandom，
;;    均不可预测，不适合用伪随机数替代。
;; 2. n 为 0 时直接返回空 bytevector，不触发系统调用。
;;
;; 错误处理
;; ----
;; n 不是精确整数时抛出 type-error；n 为负数时抛出 value-error。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 返回 bytevector 且长度与入参一致
(check (bytevector? (urandom 16)) => #t)
(check (bytevector-length (urandom 0)) => 0)
(check (bytevector-length (urandom 1)) => 1)
(check (bytevector-length (urandom 16)) => 16)
(check (bytevector-length (urandom 1024)) => 1024)

;; 0 字节返回空 bytevector
(check (urandom 0) => #u8())

;; 每个字节都是 0..255 的无符号整数
(let ((bv (urandom 32)))
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

;; 多次生成的高熵结果互不相同
(let ((a (urandom 32)) (b (urandom 32)))
  (check (equal? a b) => #f)
) ;let

;; 连续 64 次生成 16 字节数据不应出现重复
(let loop
  ((i 0) (seen '()))
  (if (>= i 64)
    (check (= (length seen) 64) => #t)
    (let ((bv (urandom 16)))
      (check (member bv seen) => #f)
      (loop (+ i 1) (cons bv seen))
    ) ;let
  ) ;if
) ;let

;; 非法参数：非精确整数抛 type-error
(check-catch 'type-error (urandom 3.14))
(check-catch 'type-error (urandom "16"))
(check-catch 'type-error (urandom #t))
(check-catch 'type-error (urandom 'abc))
(check-catch 'type-error (urandom #f))

;; 非法参数：负数抛 value-error
(check-catch 'value-error (urandom -1))
(check-catch 'value-error (urandom -1024))

(check-report)
