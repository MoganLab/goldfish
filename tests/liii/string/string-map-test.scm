(import (liii check)
        (liii string))

;; string-map
;; 将给定过程应用于字符串的每个字符，并返回新字符串，包含将过程应用于每个字符的结果。
;;
;; 语法
;; ----
;; (string-map proc str)
;;
;; 参数
;; ----
;; proc : procedure?
;; 一个函数，接收单个字符作为参数，返回转换后的字符。
;;
;; str : string?
;; 要处理的源字符串。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，包含将proc应用于str中每个字符后的结果。
;;
;; 注意
;; ----
;; string-map会创建一个新的字符串，包含将转换过程应用于每个字符的结果。
;; 空字符串会返回空字符串。
;; 谓词函数必须将每个字符映射到新的字符。
;;
;; 示例
;; ----
;; (string-map char-upcase "hello") => "HELLO"
;; (string-map char-downcase "WORLD") => "world"
;; (string-map (lambda (c) (if (char-alphabetic? c) #\X c)) "abc123") => "XXX123"
;;
;; 错误处理
;; ----
;; wrong-type-arg 当proc不是过程类型时
;; type-error 当str不是字符串类型时

; Basic functionality tests
(check (string-map char-upcase "hello world") => "HELLO WORLD")
(check (string-map char-downcase "HELLO WORLD") => "hello world")
(check (string-map char-upcase "") => "")
(check (string-map char-downcase "") => "")
(check (string-map identity "test") => "test")

; 原始测试验证
(check
  (string-map
    (lambda (ch) (integer->char (+ 1 (char->integer ch))))
    "HAL"
  )
  => "IBM"
)

; Character transformation tests
(check (string-map (lambda (c) (integer->char (- (char->integer c) 32))) "hello") => "HELLO")
(check (string-map (lambda (c) (integer->char (+ (char->integer c) 32))) "HELLO") => "hello")
(check (string-map (lambda (c) (if (char=? c #\a) #\A c)) "banana") => "bAnAnA")
(check (string-map (lambda (c) (if (char-numeric? c) #\X c)) "a1b2c3") => "aXbXcX")
(check (string-map (lambda (c) (if (char-upper-case? c) #\X #\o)) "HeLLo") => "XoXXo")

; Whitespace and special characters
(check (string-map (lambda (c) #\.) "absolute") => "........")
(check (string-map (lambda (c) (if (char-whitespace? c) #\- c)) "hello world") => "hello-world")
(check (string-map (lambda (c) (if (char-alphabetic? c) #\* c)) "test123") => "****123")

; Unicode characters (verification on byte-level)
(check (string-map char-upcase "中文english") => "中文ENGLISH")
(check (string-map (lambda (c) (if (char-alphabetic? c) #\X c)) "abc中文123") => "XXX中文123")

; Empty string handling
(check (string-map char-upcase "") => "")
(check (string-map char-downcase "") => "")
(check (string-map (lambda (c) #\a) "") => "")

; Single character handling
(check (string-map char-upcase "a") => "A")
(check (string-map char-downcase "Z") => "z")
(check (string-map (lambda (c) (integer->char (+ 1 (char->integer c)))) "a") => "b")

; Numeric handling
(check (string-map (lambda (c) (if (char-numeric? c) #\* c)) "123abc") => "***abc")
(check (string-map (lambda (c) (integer->char (+ (char->integer c) 1))) "123") => "234")
(check (string-map (lambda (c) (integer->char (- (char->integer c) 1))) "234") => "123")

; Complex transformations
(check (string-map
          (lambda (c)
            (if (even? (char->integer c))
                char-upcase
                char-downcase
            )
            c
          )
          "AbCdEf") => "AbCdEf")
(check (string-map
          (lambda (c)
            (let ((val (char->integer c)))
              (if (and (>= val 65) (<= val 90))
                  (integer->char (+ val 32))
                  (if (and (>= val 97) (<= val 122))
                      (integer->char (- val 32))
                      c
                  )
              )
            )
          )
          "Hello123World") => "hELLO123wORLD")


; Mixed case transformations
(check (string-map (lambda (c) (if (char-lower-case? c) (char-upcase c) (char-downcase c))) "HeLLo") => "hEllO")

; Identity function and no-op transformations
(check (string-map (lambda (c) c) "hello") => "hello")
(check (string-map (lambda (c) (if (char=? c #\space) #\space c)) "hello world") => "hello world")


; Whitespace preservation
(check (string-map char-upcase "  hello  world  ") => "  HELLO  WORLD  ")
(check (string-map (lambda (c) (if (char-whitespace? c) #\_ c)) "  hello  world  ") => "__hello__world__")

; Special escape character handling
(check (string-map (lambda (c) #\newline) "test") =>
"\n\n\n\n")
(check (string-map (lambda (c) (integer->char 10)) "abc") =>
"\n\n\n")

; Error handling tests
(check-catch 'wrong-type-arg (string-map 123 "hello"))
(check-catch 'wrong-type-arg (string-map char-upcase 123))
(check-catch 'wrong-type-arg (string-map "not-function" "hello"))

; Long string handling
(check (string-map char-upcase (make-string 100 #\a)) => (make-string 100 #\A))
(check (string-map char-downcase (make-string 100 #\Z)) => (make-string 100 #\z))

; Unicode string tests
(check (string-map char-upcase "cafe latte") => "CAFE LATTE")
(check (string-map char-downcase "CAFE LATTE") => "cafe latte")

(check-report)
