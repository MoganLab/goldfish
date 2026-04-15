(import (liii check)
  (liii base)
  (liii string)
) ;import

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

(check (string-map char-upcase "hello world")
  =>
  "HELLO WORLD"
) ;check
(check (string-map char-downcase "HELLO WORLD")
  =>
  "hello world"
) ;check
(check (string-map char-upcase "")
  =>
  ""
) ;check
(check (string-map char-downcase "")
  =>
  ""
) ;check
(check (string-map identity "test")
  =>
  "test"
) ;check

(check (string-map (lambda (ch)
                     (integer->char (+ 1 (char->integer ch)))
                   ) ;lambda
         "HAL"
       ) ;string-map
  =>
  "IBM"
) ;check

(check (string-map (lambda (c)
                     (integer->char (- (char->integer c) 32))
                   ) ;lambda
         "hello"
       ) ;string-map
  =>
  "HELLO"
) ;check
(check (string-map (lambda (c)
                     (integer->char (+ (char->integer c) 32))
                   ) ;lambda
         "HELLO"
       ) ;string-map
  =>
  "hello"
) ;check
(check (string-map (lambda (c) (if (char=? c #\a) #\A c))
         "banana"
       ) ;string-map
  =>
  "bAnAnA"
) ;check
(check (string-map (lambda (c)
                     (if (char-numeric? c) #\X c)
                   ) ;lambda
         "a1b2c3"
       ) ;string-map
  =>
  "aXbXcX"
) ;check
(check (string-map (lambda (c)
                     (if (char-upper-case? c) #\X #\o)
                   ) ;lambda
         "HeLLo"
       ) ;string-map
  =>
  "XoXXo"
) ;check

(check (string-map (lambda (c) #\.) "absolute")
  =>
  "........"
) ;check
(check (string-map (lambda (c)
                     (if (char-whitespace? c) #\- c)
                   ) ;lambda
         "hello world"
       ) ;string-map
  =>
  "hello-world"
) ;check
(check (string-map (lambda (c)
                     (if (char-alphabetic? c) #\* c)
                   ) ;lambda
         "test123"
       ) ;string-map
  =>
  "****123"
) ;check

(check (string-map char-upcase "中文english")
  =>
  "中文ENGLISH"
) ;check
(check (string-map (lambda (c)
                     (if (char-alphabetic? c) #\X c)
                   ) ;lambda
         "abc中文123"
       ) ;string-map
  =>
  "XXX中文123"
) ;check

(check (string-map char-upcase "")
  =>
  ""
) ;check
(check (string-map char-downcase "")
  =>
  ""
) ;check
(check (string-map (lambda (c) #\a) "")
  =>
  ""
) ;check

(check (string-map char-upcase "a")
  =>
  "A"
) ;check
(check (string-map char-downcase "Z")
  =>
  "z"
) ;check
(check (string-map (lambda (c)
                     (integer->char (+ 1 (char->integer c)))
                   ) ;lambda
         "a"
       ) ;string-map
  =>
  "b"
) ;check

(check (string-map (lambda (c)
                     (if (char-numeric? c) #\* c)
                   ) ;lambda
         "123abc"
       ) ;string-map
  =>
  "***abc"
) ;check
(check (string-map (lambda (c)
                     (integer->char (+ (char->integer c) 1))
                   ) ;lambda
         "123"
       ) ;string-map
  =>
  "234"
) ;check
(check (string-map (lambda (c)
                     (integer->char (- (char->integer c) 1))
                   ) ;lambda
         "234"
       ) ;string-map
  =>
  "123"
) ;check

(check (string-map (lambda (c)
                     (if (even? (char->integer c))
                       char-upcase
                       char-downcase
                     ) ;if
                     c
                   ) ;lambda
         "AbCdEf"
       ) ;string-map
  =>
  "AbCdEf"
) ;check
(check (string-map (lambda (c)
                     (let ((val (char->integer c)))
                       (if (and (>= val 65) (<= val 90))
                         (integer->char (+ val 32))
                         (if (and (>= val 97) (<= val 122))
                           (integer->char (- val 32))
                           c
                         ) ;if
                       ) ;if
                     ) ;let
                   ) ;lambda
         "Hello123World"
       ) ;string-map
  =>
  "hELLO123wORLD"
) ;check


(check (string-map (lambda (c)
                     (if (char-lower-case? c)
                       (char-upcase c)
                       (char-downcase c)
                     ) ;if
                   ) ;lambda
         "HeLLo"
       ) ;string-map
  =>
  "hEllO"
) ;check

(check (string-map (lambda (c) c) "hello")
  =>
  "hello"
) ;check
(check (string-map (lambda (c)
                     (if (char=? c #\space) #\space c)
                   ) ;lambda
         "hello world"
       ) ;string-map
  =>
  "hello world"
) ;check


(check (string-map char-upcase
         "  hello  world  "
       ) ;string-map
  =>
  "  HELLO  WORLD  "
) ;check
(check (string-map (lambda (c)
                     (if (char-whitespace? c) #\_ c)
                   ) ;lambda
         "  hello  world  "
       ) ;string-map
  =>
  "__hello__world__"
) ;check

(check (string-map (lambda (c) #\newline)
         "test"
       ) ;string-map
  =>
  "\n\n\n\n"
) ;check
(check (string-map (lambda (c) (integer->char 10))
         "abc"
       ) ;string-map
  =>
  "\n\n\n"
) ;check

(check-catch 'wrong-type-arg
  (string-map 123 "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-map char-upcase 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-map "not-function" "hello")
) ;check-catch

(check (string-map char-upcase
         (make-string 100 #\a)
       ) ;string-map
  =>
  (make-string 100 #\A)
) ;check
(check (string-map char-downcase
         (make-string 100 #\Z)
       ) ;string-map
  =>
  (make-string 100 #\z)
) ;check

(check (string-map char-upcase "cafe latte")
  =>
  "CAFE LATTE"
) ;check
(check (string-map char-downcase "CAFE LATTE")
  =>
  "cafe latte"
) ;check

(check-report)
