;; token-urlsafe
;; 生成符合 RFC 4648 §5 的 Base64URL 随机字符串，可直接用作 OAuth2 PKCE
;; 的 code_verifier。
;;
;; 语法
;; ----
;; (token-urlsafe [nbytes])
;;
;; 参数
;; ----
;; nbytes : exact-integer?
;; 随机字节数，默认 32；必须是非负精确整数。
;;
;; 返回值
;; -----
;; string
;; 不含 '=' 填充的 URL-Safe Base64 字符串，字符集为 [A-Za-z0-9_-]。
;;
;; 说明
;; ----
;; 1. 底层使用标准 Base64 编码，再把 '+' 替换为 '-'、'/' 替换为 '_'，
;;    并剥除尾部 '=' 填充，符合 RFC 4648 §5 的 URL-Safe 字母表。
;; 2. 默认 32 字节对应 43 个字符，天然满足 RFC 7636 所要求的
;;    code_verifier 长度区间 [43, 128] 与字符集规范。
;;
;; 错误处理
;; ----
;; nbytes 不是精确整数时抛出 type-error；nbytes 为负数时抛出 value-error。

(import (liii check) (liii secrets) (liii string))

(check-set-mode! 'report-failed)

(define (urlsafe-char? c)
  (let ((n (char->integer c)))
    (or (and (>= n 65) (<= n 90))
      (and (>= n 97) (<= n 122))
      (and (>= n 48) (<= n 57))
      (= n 45)
      (= n 95)
    ) ;or
  ) ;let
) ;define

;; 默认生成 43 个字符（32 字节的 Base64 去掉填充）
(check (string? (token-urlsafe)) => #t)
(check (string-length (token-urlsafe)) => 43)

;; 长度符合 Base64 编码规则：ceil(4 * nbytes / 3)
(check (string-length (token-urlsafe 0)) => 0)
(check (string-length (token-urlsafe 1)) => 2)
(check (string-length (token-urlsafe 16)) => 22)
(check (string-length (token-urlsafe 24)) => 32)
(check (string-length (token-urlsafe 32)) => 43)
(check (string-length (token-urlsafe 64)) => 86)

;; 0 字节返回空字符串
(check (token-urlsafe 0) => "")

;; 字符集必须是 URL-Safe Base64 字母表，且不含 '='、'+'、'/'
(let ((s (token-urlsafe 32)))
  (check (string-every urlsafe-char? s) => #t)
  (check (string-contains? s "=") => #f)
  (check (string-contains? s "+") => #f)
  (check (string-contains? s "/") => #f)
) ;let

;; 多次生成结果互不相同
(let ((a (token-urlsafe 32)) (b (token-urlsafe 32)))
  (check (string=? a b) => #f)
) ;let

;; 参数个数错误
(check-catch 'wrong-number-of-args (token-urlsafe 1 2))

;; 非法参数
(check-catch 'type-error (token-urlsafe 3.14))
(check-catch 'type-error (token-urlsafe "16"))
(check-catch 'type-error (token-urlsafe #t))
(check-catch 'value-error (token-urlsafe -1))

(check-report)
