(import (liii check) (liii string))

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

(check (let ((result '()))
         (string-for-each (lambda (c)
                            (set! result (cons c result))
                          ) ;lambda
           "abc"
         ) ;string-for-each
         result
       ) ;let
  =>
  '(#\c #\b #\a)
) ;check

(check (let ((count 0))
         (string-for-each (lambda (c) (set! count (+ count 1)))
           "hello"
         ) ;string-for-each
         count
       ) ;let
  =>
  5
) ;check

(check (let ((sum 0))
         (string-for-each (lambda (c)
                            (set! sum (+ sum (char->integer c)))
                          ) ;lambda
           "ABC"
         ) ;string-for-each
         sum
       ) ;let
  =>
  198
) ;check

(check (let ((result 0))
         (string-for-each (lambda (c) (set! result 999))
           ""
         ) ;string-for-each
         result
       ) ;let
  =>
  0
) ;check

(check (let ((char-list '()))
         (string-for-each (lambda (c)
                            (set! char-list (cons c char-list))
                          ) ;lambda
           "X"
         ) ;string-for-each
         char-list
       ) ;let
  =>
  '(#\X)
) ;check

(check (let ((whitespace-count 0))
         (string-for-each (lambda (c)
                            (when (char-whitespace? c)
                              (set! whitespace-count
                                (+ whitespace-count 1)
                              ) ;set!
                            ) ;when
                          ) ;lambda
           "hello world\n"
         ) ;string-for-each
         whitespace-count
       ) ;let
  =>
  2
) ;check

(check (let ((alphas '()) (digits '()))
         (string-for-each (lambda (c)
                            (if (char-alphabetic? c)
                              (set! alphas (cons c alphas))
                              (set! digits (cons c digits))
                            ) ;if
                          ) ;lambda
           "a1b2c3"
         ) ;string-for-each
         (list (reverse alphas) (reverse digits))
       ) ;let
  =>
  '((#\a #\b #\c) (#\1 #\2 #\3))
) ;check

(check (let ((all-chars '()))
         (string-for-each (lambda (c)
                            (set! all-chars (cons c all-chars))
                          ) ;lambda
           "中文english"
         ) ;string-for-each
         (> (length all-chars) 8)
       ) ;let
  =>
  #t
) ;check

(check (let ((chars '()) (count 0))
         (string-for-each (lambda (c)
                            (set! chars (cons c chars))
                            (set! count (+ count 1))
                          ) ;lambda
           "test"
         ) ;string-for-each
         (list (reverse chars) count)
       ) ;let
  =>
  '((#\t #\e #\s #\t) 4)
) ;check

(check (let ((tracker (make-string 3 #\a)))
         (string-for-each (lambda (c)
                            (set! tracker
                              (string-append tracker (string c))
                            ) ;set!
                          ) ;lambda
           "xyz"
         ) ;string-for-each
         (> (string-length tracker) 3)
       ) ;let
  =>
  #t
) ;check

(check-catch 'wrong-type-arg
  (string-for-each 123 "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-for-each (lambda (x) x) 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-for-each "not-function" "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-for-each char-upcase 123)
) ;check-catch

(check (let ((ascii-sum 0))
         (string-for-each (lambda (c)
                            (set! ascii-sum
                              (+ ascii-sum (char->integer c))
                            ) ;set!
                          ) ;lambda
           "Hello"
         ) ;string-for-each
         (>= ascii-sum 500)
       ) ;let
  =>
  #t
) ;check

(check (let ((upper-chars '()))
         (string-for-each (lambda (c)
                            (set! upper-chars
                              (cons (char-upcase c) upper-chars)
                            ) ;set!
                          ) ;lambda
           "abc"
         ) ;string-for-each
         (reverse upper-chars)
       ) ;let
  =>
  '(#\A #\B #\C)
) ;check

(check (let ((char-count 0))
         (string-for-each (lambda (c)
                            (set! char-count (+ char-count 1))
                          ) ;lambda
           (make-string 1000 #\x)
         ) ;string-for-each
         char-count
       ) ;let
  =>
  1000
) ;check

(check (let ((vowel-count 0))
         (string-for-each (lambda (c)
                            (when (member c
                                    '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)
                                  ) ;member
                              (set! vowel-count (+ vowel-count 1))
                            ) ;when
                          ) ;lambda
           "Hello World"
         ) ;string-for-each
         vowel-count
       ) ;let
  =>
  3
) ;check

(check (let ((lst '()))
         (string-for-each (lambda (x)
                            (set! lst
                              (cons (- (char->integer x)
                                      (char->integer #\0)
                                    ) ;-
                                lst
                              ) ;cons
                            ) ;set!
                          ) ;lambda
           "12345"
         ) ;string-for-each
         lst
       ) ;let
  =>
  '(5 4 3 2 1)
) ;check

(check (let ((lst '()))
         (string-for-each (lambda (x)
                            (set! lst
                              (cons (- (char->integer x)
                                      (char->integer #\0)
                                    ) ;-
                                lst
                              ) ;cons
                            ) ;set!
                          ) ;lambda
           "123"
         ) ;string-for-each
         lst
       ) ;let
  =>
  '(3 2 1)
) ;check

(check (let ((lst '()))
         (string-for-each (lambda (x)
                            (set! lst
                              (cons (- (char->integer x)
                                      (char->integer #\0)
                                    ) ;-
                                lst
                              ) ;cons
                            ) ;set!
                          ) ;lambda
           ""
         ) ;string-for-each
         lst
       ) ;let
  =>
  '()
) ;check

(check-report)
