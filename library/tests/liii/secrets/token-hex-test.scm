;; token-hex
;; 生成十六进制小写随机字符串，适合作为防重放 Nonce 与 CSRF Token。
;;
;; 语法
;; ----
;; (token-hex [nbytes])
;;
;; 参数
;; ----
;; nbytes : exact-integer?
;; 随机字节数，默认 32；必须是非负精确整数。
;;
;; 返回值
;; -----
;; string
;; 长度为 2 * nbytes 的十六进制小写字符串，字符集为 [0-9a-f]。
;;
;; 说明
;; ----
;; 1. 每个随机字节编码为两位十六进制字符，故字符串长度为字节数的两倍。
;; 2. 默认 32 字节对应 64 个十六进制字符（256 位熵）。
;;
;; 错误处理
;; ----
;; nbytes 不是精确整数时抛出 type-error；nbytes 为负数时抛出 value-error。

(import (liii check) (liii secrets) (liii string))

(check-set-mode! 'report-failed)

(define (hex-char? c)
  (let ((n (char->integer c)))
    (or (and (>= n 48) (<= n 57)) (and (>= n 97) (<= n 102)))
  ) ;let
) ;define

;; 默认生成 64 个十六进制字符
(check (string? (token-hex)) => #t)
(check (string-length (token-hex)) => 64)

;; 长度恒为字节数的两倍
(check (string-length (token-hex 0)) => 0)
(check (string-length (token-hex 1)) => 2)
(check (string-length (token-hex 16)) => 32)
(check (string-length (token-hex 64)) => 128)

;; 0 字节返回空字符串
(check (token-hex 0) => "")

;; 字符集必须是十六进制小写
(let ((s (token-hex 32)))
  (check (string-every hex-char? s) => #t)
  ;; 大写十六进制字符不应出现
  (check
    (string-every (lambda (c) (<= (char->integer c) 122)) s)
    =>
    #t
  ) ;check
) ;let

;; 多次生成结果互不相同
(let ((a (token-hex 32)) (b (token-hex 32)))
  (check (string=? a b) => #f)
) ;let

;; 参数个数错误
(check-catch 'wrong-number-of-args (token-hex 1 2))

;; 非法参数
(check-catch 'type-error (token-hex 3.14))
(check-catch 'type-error (token-hex "16"))
(check-catch 'type-error (token-hex #t))
(check-catch 'value-error (token-hex -1))

(check-report)
