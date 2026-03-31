(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; string-ref
;; 按索引访问字符串中的字符。
;;
;; 语法
;; ----
;; (string-ref string k)
;;
;; 参数
;; ----
;; string : string?
;; 要访问的字符串
;;
;; k : exact?
;; 必须是精确的整数索引，从0开始。必须满足 0 <= k < (string-length string)
;;
;; 返回值
;; ------
;; char?
;; 字符串中位置k处的字符
;;
;; 说明
;; ----
;; 1. 从0开始索引
;; 2. 在R7RS中，索引k必须在有效范围内
;; 3. 返回k位置处的字符
;;
;; 错误处理
;; --------
;; out-of-range
;; 当k为负数或大于等于字符串长度时抛出错误。
;;
;; wrong-type-arg
;; 当string不是字符串或k不是精确整数时抛出错误。
;;
;; 错误
;; ----
;; 当索引超出范围时，会抛出out-of-range异常。

(check (string-ref "MathAgape" 0) => #\M)
(check (string-ref "MathAgape" 2) => #\t)
(check (string-ref "hello" 0) => #\h)
(check (string-ref "hello" 4) => #\o)
(check (string-ref "a" 0) => #\a)
;; 边界测试
(check (string-ref "z" 0) => #\z)
(check (string-ref "ABC" 0) => #\A)
(check (string-ref "ABC" 2) => #\C)

;; 特殊字符测试
(check (string-ref "!@#" 0) => #\!)
(check (string-ref "123" 0) => #\1)
(check (string-ref "   " 1) => #\space)

;; ASCII边界测试
(check (string-ref "xyz" 0) => #\x)
(check (string-ref "xyz" 2) => #\z)

;; 错误处理
(check-catch 'out-of-range (string-ref "MathAgape" -1))
(check-catch 'out-of-range (string-ref "MathAgape" 9))
(check-catch 'out-of-range (string-ref "" 0))
(check-catch 'out-of-range (string-ref "abc" 3))
(check-catch 'out-of-range (string-ref "a" 1))

(check-catch 'wrong-type-arg (string-ref 123 0))
(check-catch 'wrong-type-arg (string-ref "hello" 1.5))
(check-catch 'wrong-number-of-args (string-ref "hello"))
(check-catch 'wrong-number-of-args (string-ref "hello" 1 2))

(check (string-append "Math" "Agape") => "MathAgape")

(check (string-append) => "")

(check (make-vector 1 1) => (vector 1))
(check (make-vector 3 'a) => (vector 'a 'a 'a))

(check (make-vector 0) => (vector ))
(check (vector-ref (make-vector 1) 0) => #<unspecified>)

(check (vector 'a 'b 'c) => #(a b c))
(check (vector) => #())

(check (vector? #(1 2 3)) => #t)
(check (vector? #()) => #t)
(check (vector? '(1 2 3)) => #f)

(check (vector-length #(1 2 3)) => 3)
(check (vector-length #()) => 0)

(let ((v #(1 2 3)))
  (check (vector-ref v 0) => 1)
  (check (v 0) => 1)

  (check (vector-ref v 2) => 3)
  (check (v 2) => 3)
) ;let

(check-catch 'out-of-range (vector-ref #(1 2 3) 3))
(check-catch 'out-of-range (vector-ref #() 0))
  
(check-catch 'wrong-type-arg (vector-ref #(1 2 3) 2.0))
(check-catch 'wrong-type-arg (vector-ref #(1 2 3) "2"))

(define my-vector #(0 1 2 3))
(check my-vector => #(0 1 2 3))

(check (vector-set! my-vector 2 10) => 10)
(check my-vector => #(0 1 10 3))

(check-catch 'out-of-range (vector-set! my-vector 4 10))

(check (vector->list #()) => '())
(check (vector->list #() 0) => '())

(check-catch 'out-of-range (vector->list #() 1))

(check (vector->list #(0 1 2 3)) => '(0 1 2 3))
(check (vector->list #(0 1 2 3) 1) => '(1 2 3))
(check (vector->list #(0 1 2 3) 1 1) => '())
(check (vector->list #(0 1 2 3) 1 2) => '(1))

(check (list->vector '(0 1 2 3)) => #(0 1 2 3))
(check (list->vector '()) => #())

(check-report)
