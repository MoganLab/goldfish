(define-library (liii base64)
  (import (scheme base) (liii base) (liii bitwise) (liii error) (liii string))
  (export string-base64-encode bytevector-base64-encode base64-encode
    string-base64-decode bytevector-base64-decode base64-decode base64url-encode
    base64url-decode
  ) ;export
  (begin
    (define (bytevector-base64-encode bv)
      (when (not (bytevector? bv))
        (type-error "bytevector-base64-encode: input must be bytevector" bv)
      ) ;when
      (g_bytevector-base64-encode bv)
    ) ;define

    (define (string-base64-encode str)
      (when (not (string? str))
        (type-error "string-base64-encode: input must be string" str)
      ) ;when
      (utf8->string (bytevector-base64-encode (string->utf8 str)))
    ) ;define

    (define (base64-encode x)
      (cond ((string? x) (string-base64-encode x))
            ((bytevector? x) (bytevector-base64-encode x))
            (else (type-error "input must be string or bytevector" x))
      ) ;cond
    ) ;define

    (define (bytevector-base64-decode bv)
      (when (not (bytevector? bv))
        (type-error "bytevector-base64-decode: input must be bytevector" bv)
      ) ;when
      (g_bytevector-base64-decode bv)
    ) ;define

    (define (string-base64-decode str)
      (when (not (string? str))
        (type-error "string-base64-decode: input must be string" str)
      ) ;when
      (utf8->string (bytevector-base64-decode (string->utf8 str)))
    ) ;define

    (define (base64-decode x)
      (cond ((string? x) (string-base64-decode x))
            ((bytevector? x) (bytevector-base64-decode x))
            (else (type-error "input must be string or bytevector" x))
      ) ;cond
    ) ;define

    ;; RFC 4648 §5 base64url 字母表：A-Z a-z 0-9 - _（ASCII 范围判断）。
    (define (%base64url-char? c)
      (let ((n (char->integer c)))
        (or (<= 65 n 90) (<= 97 n 122) (<= 48 n 57) (= n 45) (= n 95))
      ) ;let
    ) ;define

    ;; 标准 Base64 字符映射为 base64url 字符：+ -> -，/ -> _。
    (define (%b64->b64url-char c)
      (cond ((char=? c #\+) #\-)
            ((char=? c #\/) #\_)
            (else c)
      ) ;cond
    ) ;define

    ;; base64url 字符映射回标准 Base64 字符：- -> +，_ -> /。
    (define (%b64url->b64-char c)
      (cond ((char=? c #\-) #\+)
            ((char=? c #\_) #\/)
            (else c)
      ) ;cond
    ) ;define

    ;; base64url-encode: RFC 4648 §5 URL 安全 Base64 编码。
    ;; 按 §3.2 省略 '=' 填充，与 JWT（RFC 7515）、PKCE（RFC 7636）惯例一致。
    (define (base64url-encode bv)
      (when (not (bytevector? bv))
        (type-error "base64url-encode: input must be bytevector" bv)
      ) ;when
      (let ((b64 (utf8->string (bytevector-base64-encode bv))))
        (string-map %b64->b64url-char
          (string-trim-right b64 (lambda (c) (char=? c #\=)))
        ) ;string-map
      ) ;let
    ) ;define

    ;; base64url-decode: RFC 4648 §5 URL 安全 Base64 解码。
    ;; 兼容带 '=' 填充与不带填充的输入；若带填充，必须恰好等于规范化
    ;; 填充量（与 Go encoding/base64、主流 JWT 实现一致）。
    (define (base64url-decode str)
      (when (not (string? str))
        (type-error "base64url-decode: input must be string" str)
      ) ;when
      (let* ((body (string-trim-right str (lambda (c) (char=? c #\=))))
             (n (string-length body))
             (r (remainder n 4))
             (pad (- (string-length str) n))
             (expected (if (= r 0) 0 (- 4 r)))
            ) ;
        (if
          (or (not (string-every %base64url-char? body))
            (= r 1)
            (and (> pad 0) (not (= pad expected)))
          ) ;or
          (value-error "base64url-decode: invalid base64url input" str)
          (bytevector-base64-decode (string->utf8 (string-append (string-map %b64url->b64-char body) (make-string expected #\=))
                                    ) ;string->utf8
          ) ;bytevector-base64-decode
        ) ;if
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
