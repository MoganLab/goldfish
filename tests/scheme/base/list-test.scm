(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list 基本构造功能测试
(check (list) => '())
(check (list 'a) => '(a))
(check (list 1 2 3) => '(1 2 3))
(check (list "hello") => '("hello"))
;; 边界条件测试
(check (list) => '())
(check (list 'single) => '(single))
(check (list 'a 'b 'c 'd) => '(a b c d))
;; 数据类型兼容性测试
(check (list 42 3.14 1/2 1.0+2.0i)
  =>
  '(42 3.14 1/2 1.0+2.0i)
) ;check
(check (list #t #f) => '(#t #f))
(check (list #\a #\b #\c)
  =>
  '(#\a #\b #\c)
) ;check
(check (list "hello" "world")
  =>
  '("hello" "world")
) ;check
(check (list 'symbol1 'symbol2 'keyword:)
  =>
  '(symbol1 symbol2 keyword:)
) ;check
;; 嵌套结构测试
(check (list '() '() '())
  =>
  '(() () ())
) ;check
(check (list '(a b) '(c d))
  =>
  '((a b) (c d))
) ;check
(check (list (list 1 2) (list 3 4))
  =>
  '((1 2) (3 4))
) ;check
;; 复杂对象测试
(check (list #(1 2 3) #u(255 128))
  =>
  '(#(1 2 3) #u(255 128))
) ;check
(check (list #t #f) => '(#t #f))
(check (list 1 2 3) => '(1 2 3))
;; 混合类型测试
(check (list 'symbol 42 "text" #\c #t #(1 2))
  =>
  '(symbol 42 "text" #\c #t #(1 2))
) ;check
(check (list 'a 1 "hello" '(sub list) #t 3.14)
  =>
  '(a 1 "hello" (sub list) #t 3.14)
) ;check
;; 极大参数数量测试
(check (list 1
         2
         3
         4
         5
         6
         7
         8
         9
         10
         11
         12
         13
         14
         15
         16
         17
         18
         19
         20
       ) ;list
  =>
  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
) ;check
;; 重复元素测试
(check (list 'a 'a 'a) => '(a a a))
(check (list 1 1 1 1) => '(1 1 1 1))
(check (list "test" "test")
  =>
  '("test" "test")
) ;check
;; Unicode和特殊字符测试
(check (list "中文" "测试" "字符串")
  =>
  '("中文" "测试" "字符串")
) ;check
(check (list #\中 #\文)
  =>
  '(#\中 #\文)
) ;check
;; 深层嵌套结构测试
(check (list (list (list 'a)) (list 'b))
  =>
  '(((a)) (b))
) ;check
(check (list '() (list '() (list 'a)))
  =>
  '(() (() (a)))
) ;check
;; 性能验证：大列表构造
(check (length (list 'a
                 'b
                 'c
                 'd
                 'e
                 'f
                 'g
                 'h
                 'i
                 'j
                 'k
                 'l
                 'm
                 'n
                 'o
                 'p
                 'q
                 'r
                 's
                 't
                 'u
                 'v
                 'w
                 'x
                 'y
                 'z
               ) ;list
       ) ;length
  =>
  26
) ;check
;; 错误测试 - 参数类型验证
(check (list 123) => '(123))
(check (list "string") => '("string"))
(check (list #t) => '(#t))
;; 构造后操作验证
(let ((constructed (list 1 2 3 4 5)))
  (check (length constructed) => 5)
  (check (list? constructed) => #t)
  (check (list-ref constructed 2) => 3)
) ;let
;; 独立对象验证 - 确认不与参数共享
(let ((a 'original) (b "test") (c #t))
  (let ((result (list a b c)))
    (check (equal? result '(original "test" #t))
      =>
      #t
    ) ;check
    (check (not (eq? result a)) => #t)
    (check (not (eq? result b)) => #t)
  ) ;let
) ;let
;; 参数传递验证测试
(define (test-list-wrapper . args)
  (apply list args)
) ;define
(check (test-list-wrapper 1 2 3)
  =>
  '(1 2 3)
) ;check
(check (test-list-wrapper 'a 'b 'c 'd)
  =>
  '(a b c d)
) ;check
(check (test-list-wrapper) => '())
(check-report)