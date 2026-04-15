(import (liii check) (liii uri-record))


(check-set-mode! 'report-failed)


;; 测试空格字符的编码
;; 空格 ASCII 是 32，十六进制是 20
;; 应该编码为 "%20"


;; 先看十六进制转换是否正确
(define (hex-digit n)
  (if (< n 10)
    (integer->char (+ n (char->integer #\0))
    ) ;integer->char
    (integer->char (+ (- n 10) (char->integer #\A))
    ) ;integer->char
  ) ;if
) ;define


(check (hex-digit 0) => #\0)
(check (hex-digit 2) => #\2)
(check (hex-digit 10) => #\A)
(check (hex-digit 15) => #\F)


;; 检查空格字符
(check (char->integer #\space) => 32)
(check (quotient 32 16) => 2)
(check (remainder 32 16) => 0)
(check (hex-digit 2) => #\2)
(check (hex-digit 0) => #\0)


;; 所以空格应该编码为 "%20"
(check (uri-encode " ") => "%20")
(check (uri-encode "hello world")
  =>
  "hello%20world"
) ;check


(check-report)
