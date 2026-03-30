(import (liii check)
        (liii string))

;; string-for-each
;; 将给定过程应用于字符串的每个字符，用于副作用操作，不返回有意义的值。
;;
;; 语法
;; ----
;; (string-for-each proc str)
;;
;; 参数
;; ----
;; proc : procedure?
;; 一个函数，接收单个字符作为参数，用于副作用处理。
;;
;; str : string?
;; 要处理的源字符串。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定返回值，执行只为了副作用。
;;
;; 注意
;; ----
;; string-for-each与string-map不同，它不产生新字符串，而是对每个字符执行副作用操作。
;; 常用于遍历字符串并对每个字符执行操作，如统计、打印、修改可变状态等。
;;
;; string-for-each 不支持Unicode字符，按照字节而非字符级别处理字符串。
;; 遇到中文字符等多字节字符会基于UTF-8编码字节进行处理。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当proc不是过程类型时
;; type-error 当str不是字符串类型时

; Basic functionality tests
(check
  (let ((result '()))
    (string-for-each (lambda (c) (set! result (cons c result))) "abc")
    result
  )
  => '(#\c #\b #\a)
)

(check
  (let ((count 0))
    (string-for-each (lambda (c) (set! count (+ count 1))) "hello")
    count
  )
  => 5
)

(check
  (let ((sum 0))
    (string-for-each
      (lambda (c) (set! sum (+ sum (char->integer c))))
      "ABC"
    )
    sum
  )
  => 198 ; 65+66+67
)

; Empty string handling
(check
  (let ((result 0))
    (string-for-each (lambda (c) (set! result 999)) "")
    result
  )
  => 0
)

; Single character handling
(check
  (let ((char-list '()))
    (string-for-each (lambda (c) (set! char-list (cons c char-list))) "X")
    char-list
  )
  => '(#\X)
)

; Special character handling
(check
  (let ((whitespace-count 0))
    (string-for-each
      (lambda (c) (when (char-whitespace? c) (set! whitespace-count (+ whitespace-count 1))))
      "hello world\n"
    )
    whitespace-count
  )
  => 2
)

; Numeric and alphabetic character handling
(check
  (let ((alphas '())
        (digits '()))
    (string-for-each
      (lambda (c)
        (if (char-alphabetic? c)
            (set! alphas (cons c alphas))
            (set! digits (cons c digits))
        )
      )
      "a1b2c3"
    )
    (list (reverse alphas) (reverse digits))
  )
  => '((#\a #\b #\c) (#\1 #\2 #\3))
)

; Unicode character handling
(check
  (let ((all-chars '()))
    (string-for-each (lambda (c) (set! all-chars (cons c all-chars))) "中文english")
    (> (length all-chars) 8)
  )
  => #t
)

; Multiple side effects
(check
  (let ((chars '())
        (count 0))
    (string-for-each
      (lambda (c)
        (set! chars (cons c chars))
        (set! count (+ count 1))
      )
      "test"
    )
    (list (reverse chars) count)
  )
  => '((#\t #\e #\s #\t) 4)
)

; String mutation tracking
(check
  (let ((tracker (make-string 3 #\a)))
    (string-for-each
      (lambda (c) (set! tracker (string-append tracker (string c))))
      "xyz"
    )
    (> (string-length tracker) 3)
  )
  => #t
)

; Error handling tests
(check-catch 'wrong-type-arg (string-for-each 123 "hello"))
(check-catch 'wrong-type-arg (string-for-each (lambda (x) x) 123))
(check-catch 'wrong-type-arg (string-for-each "not-function" "hello"))
(check-catch 'wrong-type-arg (string-for-each char-upcase 123))

; Complex operations
(check
  (let ((ascii-sum 0))
    (string-for-each
      (lambda (c) (set! ascii-sum (+ ascii-sum (char->integer c))))
      "Hello"
    )
    (>= ascii-sum 500)
  )
  => #t
)

; Functional conversion tracking
(check
  (let ((upper-chars '()))
    (string-for-each
      (lambda (c) (set! upper-chars (cons (char-upcase c) upper-chars)))
      "abc"
    )
    (reverse upper-chars)
  )
  => '(#\A #\B #\C)
)

; Very long string processing
(check
  (let ((char-count 0))
    (string-for-each
      (lambda (c) (set! char-count (+ char-count 1)))
      (make-string 1000 #\x)
    )
    char-count
  )
  => 1000
)

; Mixed content handling
(check
  (let ((vowel-count 0))
    (string-for-each
      (lambda (c)
        (when (member c '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))
          (set! vowel-count (+ vowel-count 1))
        )
      )
      "Hello World"
    )
    vowel-count
  )
  => 3
)

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "12345"
    )
    lst
  )
  => '(5 4 3 2 1)
)

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "123"
    )
    lst
  )
  => '(3 2 1)
)

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      ""
    )
    lst
  )
  => '()
)

(check-report)
