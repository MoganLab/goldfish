;; (liii secrets) - 密码学安全随机数工具库
;;
;; 对齐 Python PEP 506（Adding A Secrets Module To The Standard Library）的
;; 设计：底层由 (liii os) 的 urandom 提供操作系统熵源，本库只负责 Token 的
;; 编码与业务安全工具，避免开发者误用伪随机数生成安全凭据。
;;
;; 注意：本库的随机数均为密码学安全随机数（CSPRNG），不可预测；
;;       需要可复现的伪随机序列请使用 (liii random) 或 (srfi srfi-27)。

(define-library (liii secrets)
  (export token-bytes token-hex token-urlsafe randbelow choice compare-digest)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii string)
    (liii bitwise)
    (liii base64)
    (liii os)
  ) ;import
  (begin

    (define %hex-digits "0123456789abcdef")

    ;; 校验字节长度参数：必须是精确整数且非负。
    (define (%check-nbytes who nbytes)
      (cond ((not (exact-integer? nbytes))
             (type-error (string-append who ": nbytes must be an exact integer") nbytes)
            ) ;
            ((< nbytes 0)
             (value-error (string-append who ": nbytes must be non-negative") nbytes)
            ) ;
            (else nbytes)
      ) ;cond
    ) ;define

    ;; 校验上界参数：必须是精确整数且为正数。
    (define (%check-positive who n)
      (cond ((not (exact-integer? n))
             (type-error (string-append who ": argument must be an exact integer") n)
            ) ;
            ((<= n 0)
             (value-error (string-append who ": argument must be a positive integer") n)
            ) ;
            (else n)
      ) ;cond
    ) ;define

    ;; token-bytes: 生成 nbytes 字节的密码学安全随机数据，默认 32 字节。
    (define* (token-bytes (nbytes 32))
      (urandom (%check-nbytes "token-bytes" nbytes))
    ) ;define*

    ;; token-hex: 生成 2 * nbytes 长度的十六进制小写随机字符串，
    ;; 适合作为防重放 Nonce 与 CSRF Token。
    (define* (token-hex (nbytes 32))
      (let* ((nbytes (%check-nbytes "token-hex" nbytes)) (bv (urandom nbytes)))
        (let loop
          ((i 0) (acc '()))
          (if (>= i nbytes)
            (list->string (reverse acc))
            (let ((b (bytevector-u8-ref bv i)))
              (loop (+ i 1)
                (cons (string-ref %hex-digits (remainder b 16))
                  (cons (string-ref %hex-digits (quotient b 16)) acc)
                ) ;cons
              ) ;loop
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define*

    ;; token-urlsafe: 生成 RFC 4648 §5 定义的 Base64URL 随机字符串，
    ;; 严格剥除尾部 '=' 填充。默认 32 字节对应 43 个 URL-Safe 字符，
    ;; 天然满足 RFC 7636（OAuth2 PKCE）code_verifier 的 [43, 128] 长度要求。
    (define* (token-urlsafe (nbytes 32))
      (let* ((nbytes (%check-nbytes "token-urlsafe" nbytes))
             (bv (urandom nbytes))
             (encoded (utf8->string (bytevector-base64-encode bv)))
            ) ;
        (string-trim-right (string-replace (string-replace encoded "+" "-") "/" "_")
          #\=
        ) ;string-trim-right
      ) ;let*
    ) ;define*

    ;; 计算满足 256^k >= n 的最小字节数 k，用于无偏随机取样的采样宽度。
    (define (%sample-width n)
      (let loop
        ((k 1) (limit 256))
        (if (>= limit n) k (loop (+ k 1) (* limit 256)))
      ) ;let
    ) ;define

    ;; 把大端字节序列折叠为精确整数。
    (define (%bytes->integer bv)
      (let loop
        ((i 0) (acc 0))
        (if (>= i (bytevector-length bv))
          acc
          (loop (+ i 1) (+ (* acc 256) (bytevector-u8-ref bv i)))
        ) ;if
      ) ;let
    ) ;define

    ;; randbelow: 在 [0, n) 区间内产生密码学安全且无模偏差的均匀随机整数。
    ;; 采用拒绝采样：采样宽度取 256^k >= n 的最小 k，抽到 >= n 的值时丢弃重抽，
    ;; 从而保证 [0, n) 内每个整数出现的概率严格相等。
    (define (randbelow n)
      (let ((n (%check-positive "randbelow" n)))
        (let ((width (%sample-width n)))
          (let loop
            ()
            (let ((v (%bytes->integer (urandom width))))
              (if (< v n) v (loop))
            ) ;let
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;; choice: 从非空列表或向量中密码学安全地随机选取单一元素。
    (define (choice seq)
      (cond
       ((vector? seq)
        (let ((len (vector-length seq)))
          (if (= len 0)
            (value-error "choice: sequence must not be empty" seq)
            (vector-ref seq (randbelow len))
          ) ;if
        ) ;let
       ) ;
       ((list? seq)
        (let ((len (length seq)))
          (if (= len 0)
            (value-error "choice: sequence must not be empty" seq)
            (list-ref seq (randbelow len))
          ) ;if
        ) ;let
       ) ;
       (else (type-error "choice: sequence must be a list or a vector" seq))
      ) ;cond
    ) ;define

    ;; 常量时间字节级比较：完整遍历两侧内容并累积按位差异，
    ;; 只有在长度一致且差异累积为 0 时才返回 #t，
    ;; 使比较耗时与公共前缀长度无关，抵御时序旁路攻击。
    (define (%digest-equal? la lb acc)
      (and (= la lb) (= acc 0))
    ) ;define

    (define (%compare-digest-bytes length-of ref-at a b)
      (let* ((la (length-of a)) (lb (length-of b)) (limit (if (> la lb) la lb)))
        (let loop
          ((i 0) (acc 0))
          (if (>= i limit)
            (%digest-equal? la lb acc)
            (let ((ca (if (< i la) (ref-at a i) 0)) (cb (if (< i lb) (ref-at b i) 0)))
              (loop (+ i 1) (bitwise-ior acc (bitwise-xor ca cb)))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%bytevector-length-of bv)
      (bytevector-length bv)
    ) ;define

    (define (%bytevector-ref-at bv i)
      (bytevector-u8-ref bv i)
    ) ;define

    (define (%string-length-of str)
      (string-length str)
    ) ;define

    (define (%string-ref-at str i)
      (char->integer (string-ref str i))
    ) ;define

    ;; compare-digest: 常量时间比对两个字符串或两个 bytevector。
    (define (compare-digest a b)
      (cond ((and (string? a) (string? b))
             (%compare-digest-bytes %string-length-of %string-ref-at a b)
            ) ;
            ((and (bytevector? a) (bytevector? b))
             (%compare-digest-bytes %bytevector-length-of %bytevector-ref-at a b)
            ) ;
            (else (type-error "compare-digest: both arguments must be strings or both bytevectors"
                    a
                    b
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
