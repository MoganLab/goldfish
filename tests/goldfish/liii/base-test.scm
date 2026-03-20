;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

#|
eqv?
判断两个对象是否值相等，根据R7RS规范，eqv?在不同类型的数据上表现不同。

语法
----
(eqv? obj1 obj2)

参数
----
obj1, obj2 : any
任意类型的对象

返回值
-----
boolean?
如果两个对象值相等则返回 #t，否则返回 #f。

|#

;; Test eqv? for boolean values
(check-true (eqv? #t #t))
(check-true (eqv? #f #f))
(check-false (eqv? #t #f))

;; Test eqv? for exact numbers
(check-true (eqv? 42 42))
(check-false (eqv? 42 43))

;; Test eqv? for inexact numbers
(check-true (eqv? 3.14 3.14))
(check-false (eqv? 3.14 2.71))

;; Test eqv? for characters
(check-true (eqv? #\a #\a))
(check-false (eqv? #\a #\b))

;; Test eqv? for symbols
(check-true (eqv? 'abc 'abc))
(check-false (eqv? 'abc 'def))

;; Test eqv? for lists (same instance)
(check-true (let ((lst (list 1 2 3)))
              (eqv? lst lst))
) ;check-true

;; Test eqv? for lists (different instances)
(check-false (eqv? (list 1 2 3) (list 1 2 3)))

;; Test eqv? for strings (always #f due to different instances)
(check-false (eqv? "hello" "hello"))
(check-false (eqv? "hello" "world"))

;; Test eqv? for procedures
(check-true (eqv? car car))
(check-false (eqv? car cdr))

;;; eq?

#|
eq?
判断两个对象是否引用相同（对象为同一），即判断对象标识。

语法
----
(eq? obj1 obj2)

参数
----
obj1, obj2 : any
任意类型的对象

返回值
-----
boolean?
如果两个对象是同一对象则返回 #t，否则返回 #f。
|#

;; Test eq? for boolean values
(check-true (eq? #t #t))
(check-true (eq? #f #f))
(check-false (eq? #t #f))

;; Test eq? for exact numbers (may return #f for different instances)
(check-true (eq? 42 42))
(check-false (eq? 42 43))

;; Test eq? for symbols
(check-true (eq? 'abc 'abc))
(check-false (eq? 'abc 'def))

;; Test eq? for lists (not the same instance)
(check-false (eq? (list 1 2 3) (list 1 2 3)))
(check-true (let ((lst (list 1 2 3)))
              (eq? lst lst))
) ;check-true

;; Test eq? for strings (always #f due to different instances)
(check-false (eq? "hello" "hello"))

;; Test eq? for procedures
(check-true (eq? car car))
(check-false (eq? car cdr))

;;; equal?

#|
equal?
判断两个对象结构是否相等，根据R7RS规范，equal?对复杂数据结构进行深比较。

语法
----
(equal? obj1 obj2)

参数
----
obj1, obj2 : any
任意类型的对象

返回值
-----
boolean?
如果两个对象结构相等则返回 #t，否则返回 #f。
|#

;; Test equal? for simple types
(check-true (equal? #t #t))
(check-true (equal? 42 42))
(check-true (equal? 3.14 3.14))
(check-true (equal? "hello" "hello"))
(check-true (equal? 'abc 'abc))

;; Test equal? for lists
(check-true (equal? (list 1 2 3) (list 1 2 3)))
(check-false (equal? (list 1 2 3) (list 1 2 4)))

;; Test equal? for nested lists
(check-true (equal? (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4))))
(check-false (equal? (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 5))))

;; Test equal? for vectors
(check-true (equal? (vector 1 2 3) (vector 1 2 3)))
(check-false (equal? (vector 1 2 3) (vector 1 2 4)))

;; Test equal? for nested vectors
(check-true (equal? (vector (vector 1 2) (vector 3 4)) (vector (vector 1 2) (vector 3 4))))
(check-false (equal? (vector (vector 1 2) (vector 3 4)) (vector (vector 1 2) (vector 3 5))))

;; Test equal? for mixed structures
(check-true (equal? (list 1 (vector 2 3) 4) (list 1 (vector 2 3) 4)))
(check-false (equal? (list 1 (vector 2 3) 4) (list 1 (vector 2 4) 4)))

;; Test equal? for empty structures
(check-true (equal? (list) (list)))
(check-true (equal? (vector) (vector)))

;; Test equal? for different types
(check-false (equal? 42 "hello"))
(check-false (equal? #\a "a"))


(check ((lambda (x) (* x x)) 5) => 25)
(check ((lambda (x) (* x x)) 0) => 0)
(check ((lambda (x) (* x x)) -3) => 9)

(check ((lambda (x y) (+ x y)) 3 5) => 8)
(check ((lambda (x y) (* x y)) 4 6) => 24)

(check ((lambda () 42)) => 42)

(check ((lambda (x) ((lambda (y) (+ x y)) 5)) 3) => 8)

(define (apply-function f x) (f x))
(check (apply-function (lambda (x) (* x x)) 5) => 25)
(check (apply-function (lambda (x) (+ x 1)) 10) => 11)

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))
  ) ;cond
) ;define
(check (map (lambda (x) (* x 2)) '(1 2 3 4)) => '(2 4 6 8))
(check (map (lambda (x) (+ x 1)) '(0 1 2 3)) => '(1 2 3 4))

(check (filter (lambda (x) (> x 2)) '(1 2 3 4 5)) => '(3 4 5))

(check (if (> 3 2) ((lambda () 3)) ((lambda () 2))) => 3)
(check (if (< 3 2) ((lambda () 3)) ((lambda () 2))) => 2)

(check (cond ((> 3 2) ((lambda () 3))) (else ((lambda () 2)))) => 3)
(check (cond ((< 3 2) ((lambda () 3))) (else ((lambda () 2)))) => 2)

(let ((create-counter (lambda () (let ((count 0)) (lambda () (set! count (+ count 1)) count)))))
  (let ((counter1 (create-counter)) (counter2 (create-counter)))
    (counter1) (counter1) (counter2) (check (counter1) => 3) (check (counter2) => 2)
  ) ;let
) ;let

(check-catch 'unbound-variable ((lambda (x) y) 5))
(check-catch 'wrong-type-arg (map (lambda (x) (+ x 1)) '(1 2 a 4)))

(check (if (> 3 2) 3 2) => 3)
(check (if (< 3 2) 3 2) => 2)

(check (if (and (> 3 1) (< 3 4)) 'true-branch 'false-branch) => 'true-branch)
(check (if (or (> 3 4) (< 3 1)) 'true-branch 'false-branch) => 'false-branch)

(check (cond ((> 3 2) 3) (else 2)) => 3)
(check (cond ((< 3 2) 3) (else 2)) => 2)
(check (cond ((and (> 3 1) (< 3 4)) 'true-branch) (else 'false-branch)) => 'true-branch)
(check (cond ((or (> 3 4) (< 3 1)) 'true-branch) (else 'false-branch)) => 'false-branch)

(check (cond (2 => (lambda (n) (* n 2)))) => 4)
(check (cond (#f => (lambda (n) (* n 2))) (else 'no-match)) => 'no-match)
(check (cond (3 => (lambda (n) (* n 2))) (else 'no-match)) => 6)

(check (case '+
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0
) ;check

(check (case '-
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0
) ;check

(check (case '*
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p1
) ;check

(check (case '@
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>
) ;check

(check (case '&
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>
) ;check

#|
and
对任意数量的参数执行逻辑与操作，支持短路求值。

语法
----
(and [expr ...])

参数
----
expr : any
任意类型的表达式。在 Scheme 中，除了 #f 之外的所有值都被视为真值。

返回值
-----
any
如果没有任何表达式，返回 #t
如果只有一个表达式，返回该表达式的结果
对于多个表达式，返回最后一个真值表达式的结果，或者遇到第一个假值时立即返回 #f

短路求值
-------
从左到右依次求值，一旦遇到 #f 就立即停止求值并返回 #f

|#

;; 基础测试用例
(check-true (and))  ; 零参数情况

(check (and 1) => 1)  ; 单参数 - 真值
(check-false (and #f))  ; 单参数 - 假值

;; 多参数真值情况
(check-true (and #t #t #t))
(check (and 1 2 3) => 3)  ; 返回最后一个真值
(check (and #t "string" 'symbol) => 'symbol)

;; 多参数假值情况
(check-false (and #t #f #t))
(check-false (and #f #t #f))
(check-false (and #f #f #f))

;; 混合类型测试
(check-true (and 1 '() "non-empty" #t))
(check-false (and #f '() "non-empty" #t))
(check-false (and 1 '() "non-empty" #f))

;; 表达式求值测试
(check-true (and (> 5 3) (< 5 10)))
(check-false (and (> 5 3) (> 5 10)))

;; 短路求值测试
(check-catch 'error-name
  (and (error 'error-name "This should not be evaluated") #f)
) ;check-catch
(check-false (and #f (error "This should not be evaluated")))

;; 边缘情况测试
(check (and 0) => 0)  ; 0 在 Scheme 中是真值
(check (and '()) => '())  ; 空列表是真值
(check (and #t #t '()) => '())  ; 返回最后一个真值
(check-false (and #t #t #f #t))  ; 在第三个参数短路

;; 确保返回的是原始值而非转换后的布尔值
(check (and #t 42) => 42)
(check (and #t 'a 'b 'c) => 'c)
(check-false (and 'a 'b #f 'd))

(check-true (or #t #t #t))
(check-true (or #t #f #t))
(check-true (or #f #t #f))
(check-false (or #f #f #f))

(check-false (or))

(check (or 1 '() "non-empty" #t) => 1)
(check (or #f '() "non-empty" #t) => '())
(check (or 1 '() "non-empty" #f) => 1)

(check-true (or (> 5 3) (< 5 10)))
(check-true (or (> 5 3) (> 5 10)))
(check-false (or (< 5 3) (> 5 10)))

(check-true (or #t (error "This should not be evaluated")))  ; 短路，不会执行error
(check-catch 'error-name
  (or (error 'error-name "This should be evaluated") #f)  ; 第一个条件为error，不会短路
) ;check-catch


(check (or #f 1) => 1)  ; 返回第一个为真的值
(check (or #f #f 2) => 2)  ; 返回第一个为真的值
(check (or #f #f #f) => #f)  ; 所有都为假，返回假


(check (when #t 1) => 1)

(check (when #f 1 ) => #<unspecified>)

(check (when (> 3 1) 1 ) => 1)

(check (when (> 1 3) 1 ) => #<unspecified>)

(check (let ((x 1)) x) => 1)

(check (let ((x 1) (y 2)) (+ x y)) => 3)

(check (let ((x 1))
         (let ((x 2))
           x)) => 2)

(check (let ((x 1))
         (if (> x 0)
             x
             -x)) => 1)

(check (let loop ((n 5) (acc 0))
         (if (zero? n)
           acc
           (loop (- n 1) (+ acc n)))) => 15)

(check (let factorial ((n 5))
         (if (= n 1)
           1
           (* n (factorial (- n 1))))) => 120)

(check (let sum ((a 3) (b 4))
         (+ a b)) => 7)

(check (let outer ((x 2))
         (let inner ((y 3))
           (+ x y))) => 5)

;; 基础测试 - 验证顺序绑定的功能
(check
  (let* ((x 10)
         (y (+ x 5)))  ; y 可以使用之前定义的 x
    y
  ) ;let*
  => 15
) ;check

;; 多层嵌套绑定
(check
  (let* ((a 1)
         (b (+ a 1))
         (c (* b 2)))
    (* a b c)
  ) ;let*
  => 8  ; 1 * 2 * 4 = 8 
) ;check

;; 变量更新
(check
  (let* ((x 1)
         (x (+ x 1))
         (x (* x 2)))
    x
  ) ;let*
  => 4
) ;check

;; 空绑定
(check
  (let* ()
    "result"
  ) ;let*
  => "result"
) ;check

;; 作用域测试
(check
  (let* ((x 10))
    (let* ((y (+ x 5)))
      (+ x y)
    ) ;let*
  ) ;let*
  => 25
) ;check

;; 嵌套 let*
(check
  (let* ((a 1)
         (b 2))
    (let* ((c (+ a b))
           (d (* a b c)))
      (+ a b c d)
    ) ;let*
  ) ;let*
  => 12  ; 1 + 2 + 3 + (1*2*3) = 12 
) ;check

;; 闭包测试
(check
  (let ((x 1))
    (let* ((y (+ x 1))
           (z (lambda () (+ x y))))
      (z)
    ) ;let*
  ) ;let
  => 3
) ;check

;; 副作用测试
(check
  (let ((counter 0))
    (let* ((a (begin (set! counter (+ counter 1)) 10))
           (b (begin (set! counter (+ counter 1)) 20)))
      counter
    ) ;let*
  ) ;let
  => 2
) ;check

;; 类型混用
(check
  (let* ((s "Hello")
         (len (string-length s))
         (lst (cons len (cons s '()))))
    lst
  ) ;let*
  => '(5 "Hello")
) ;check

;; 错误用法测试
(check-catch 'unbound-variable
  (let* ((x y)  ; y 未定义
         (y 10))
    x
  ) ;let*
) ;check-catch

;; 复杂表达式
(check
  (let* ((x (if #t 10 20))
         (y (let* ((a x)
                   (b (+ a 5)))
              (+ a b)))
         ) ;y
    y
  ) ;let*
  => 25  ; 10 + (10+5) = 25
) ;check

(define (test-letrec)
  (letrec ((even?
             (lambda (n)
               (if (= n 0)
                   #t
                   (odd? (- n 1)))
               ) ;if
             ) ;lambda
           (odd?
            (lambda (n)
              (if (= n 0)
                  #f
                  (even? (- n 1)))
              ) ;if
            ) ;lambda
           ) ;odd?
    (list (even? 10) (odd? 10))
  ) ;letrec
) ;define

(check (test-letrec) => (list #t #f))

(check-catch 'wrong-type-arg
  (letrec ((a 1) (b (+ a 1))) (list a b))
) ;check-catch

(check
  (letrec* ((a 1) (b (+ a 1))) (list a b))
  => (list 1 2)
) ;check

(check (let-values (((ret) (+ 1 2))) (+ ret 4)) => 7)
(check (let-values (((a b) (values 3 4))) (+ a b)) => 7)

(check (and-let* ((hi 3) (ho #f)) (+ hi 1)) => #f)
(check (and-let* ((hi 3) (ho #t)) (+ hi 1)) => 4)

(check
  (do ((i 0 (+ i 1)))
      ((= i 5) i)
  ) ;do
  => 5
) ;check

(check
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((= i 5) sum)
  ) ;do
  => 10
) ;check

(check
  (do ((i 0))
      ((= i 5) i)
      (set! i (+ i 1))
  ) ;do
  => 5
) ;check

(check
  (let1 vec (make-vector 5)
    (do ((i 0 (+ i 1)))
        ((= i 5) vec)
        (vector-set! vec i i)
    ) ;do
  ) ;let1
  => #(0 1 2 3 4)
) ;check

(define* (hi a (b 32) (c "hi")) (list a b c))

(check (hi 1) => '(1 32 "hi"))
(check (hi :b 2 :a 3) => '(3 2 "hi"))
(check (hi 3 2 1) => '(3 2 1))

(define* (g a (b a) (k (* a b)))
  (list a b k)
) ;define*

(check (g 3 4) => '(3 4 12))
(check (g 3 4 :k 5) => '(3 4 5))

(let ()
  (define-values (value1 value2) (values 1 2))
  (check value1 => 1)
  (check value2 => 2)
) ;let

(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr)
) ;define-record-type

(check (pare? (kons 1 2)) => #t)
(check (pare? (cons 1 2)) => #f)
(check (kar (kons 1 2)) => 1)
(check (kdr (kons 1 2)) => 2)

(check
 (let ((k (kons 1 2)))
   (set-kar! k 3)
   (kar k)
 ) ;let
 => 3
) ;check

(define-record-type :person
  (make-person name age)
  person?
  (name get-name set-name!)
  (age get-age)
) ;define-record-type

(check (person? (make-person "Da" 3)) => #t)
(check (get-age (make-person "Da" 3)) => 3)
(check (get-name (make-person "Da" 3)) => "Da")
(check
  (let ((da (make-person "Da" 3)))
    (set-name! da "Darcy")
    (get-name da)
  ) ;let
  => "Darcy"
) ;check

#|
number?
判断一个对象是否是数（包括整数、浮点数、有理数、复数）。

语法
----
(number? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
如果 obj 是数值类型（整数、浮点数、有理数、复数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (number? 123))          ; 整数
(check-true (number? 123.456))      ; 浮点数
(check-true (number? 1/2))          ; 有理数
(check-true (number? 1+2i))         ; 复数
(check-false (number? "123"))       ; 字符串
(check-false (number? #t))          ; 布尔值
(check-false (number? 'symbol))     ; 符号
(check-false (number? '(1 2 3)))    ; 列表

#|
complex?
判断一个对象是否是复数（包括整数、浮点数、有理数、复数）。

语法
----
(complex? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
如果 obj 是数值类型（整数、浮点数、有理数、复数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (complex? 1+2i))        ; 复数
(check-true (complex? 123))         ; 整数也是复数
(check-true (complex? 123.456))     ; 浮点数也是复数
(check-true (complex? 1/2))         ; 有理数也是复数
(check-false (complex? "123"))      ; 字符串
(check-false (complex? #t))         ; 布尔值
(check-false (complex? 'symbol))    ; 符号
(check-false (complex? '(1 2 3)))   ; 列表

#|
real?
判断一个对象是否实数（包括整数、浮点数、有理数）。

语法
----
(real? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
如果 obj 是数值类型（整数、浮点数、有理数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (real? 123))            ; 整数
(check-true (real? 123.456))        ; 浮点数
(check-true (real? 1/2))            ; 有理数
(check-false (real? 1+2i))          ; 复数
(check-false (real? "123"))         ; 字符串
(check-false (real? #t))            ; 布尔值
(check-false (real? 'symbol))       ; 符号
(check-false (real? '(1 2 3)))      ; 列表

#|
rational?
判断一个对象是否是有理数（包括整数、有理数）。

语法
----
(rational? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
如果 obj 是数值类型（整数、有理数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (rational? 123))        ; 整数
(check-true (rational? 1/2))        ; 有理数
(check-false (rational? 123.456))   ; 浮点数
(check-false (rational? 1+2i))      ; 复数
(check-false (rational? "123"))     ; 字符串
(check-false (rational? #t))        ; 布尔值
(check-false (rational? 'symbol))   ; 符号
(check-false (rational? '(1 2 3)))  ; 列表

#|
integer?
判断一个对象是否是整数（包括整数）。

语法
----
(integer? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
如果 obj 是数值类型（整数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (integer? 123))         ; 整数
(check-false (integer? 123.456))    ; 浮点数
(check-false (integer? 1/2))        ; 有理数
(check-false (integer? 1+2i))       ; 复数
(check-false (integer? "123"))      ; 字符串
(check-false (integer? #t))         ; 布尔值
(check-false (integer? 'symbol))    ; 符号
(check-false (integer? '(1 2 3)))   ; 列表

#|
exact-integer?
判断一个数值是否为精确整数。

语法
----
(exact-integer? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
如果 obj 是精确数值且为整数返回 #t，否则返回 #f。

|#

(check-true (exact-integer? 42))        ; 精确整数
(check-true (exact-integer? -42))       ; 精确负数
(check-true (exact-integer? 0))         ; 零
;(check-true (exact-integer? #e42.0))   精确浮点数转换为整数(暂不支持此类数)
(check-false (exact-integer? 42.0))     ; 不精确整数
(check-false (exact-integer? 1/2))      ; 有理数
(check-false (exact-integer? 3.14))     ; 不精确浮点数
(check-false (exact-integer? 1+2i))     ; 复数
(check-false (exact-integer? "42"))     ; 字符串
(check-false (exact-integer? #t))       ; 布尔值
(check-false (exact-integer? 'symbol))  ; 符号

#|
exact?
判断一个数是否是精确数。

语法
----
(exact? obj)

参数
----
obj : number?
任意数值类型的对象。

返回值
-----
boolean?
如果 obj 是精确数（整数、有理数、精确浮点数）返回 #t，否则返回 #f。

错误
----
无错误情况。

|#

(check-true (exact? 1))
(check-true (exact? 1/2))
(check-false (exact? 0.3))
; (check-true (exact? #e3.0))

#|
inexact?
用于判断一个数值是否为不精确值。

语法
----
(inexact? obj)

参数
----
obj : number?
任何数值类型的对象

返回值
-----
boolean?
如果 obj 是不精确数（不精确的浮点数、运算结果中的不精确部分、复数的任何部分是不精确的等）返回 #t，否则返回 #f。

说明
----
1. 整数和有理数（精确分数）通常返回 #f，表示它们是精确的
2. 浮点数和运算中涉及不精确数的表达式通常返回 #t
3. 对于复数，如果实部或虚部任何一部分是不精确的，则返回 #t
4. 特殊数值如无穷大和NaN返回 #t
5. 精确浮点数（使用精确前缀）返回 #f

错误处理
--------
wrong-type-arg
如果参数不是数字类型，抛出错误。

|#

;; 基本测试
(check-false (inexact? 42))             ;整数是精确的
(check-false (inexact? 3/4))            ;有理数是精确的
(check-true (inexact? 3.14))            ;浮点数是不精确的
(check-true (inexact? 1.0e3))           ;科学计数法是不精确的
(check-true (inexact? 1+2i))            ;复数通常是不精确的
(check-true (inexact? +inf.0))          ;特殊数值是不精确的
(check-true (inexact? -inf.0))          ;特殊数值是不精确的
(check-true (inexact? +nan.0))          ;NaN是不精确的

;; 精确值测试
(check-false (inexact? 0))
(check-false (inexact? 1))
(check-false (inexact? -1))
(check-false (inexact? 1000000))
(check-false (inexact? -1000000))
(check-false (inexact? 1/2))
(check-false (inexact? 1/3))
(check-false (inexact? 5/3))
(check-false (inexact? -1/2))
(check-false (inexact? -5/7))

;; 不精确值测试
(check-true (inexact? 0.0))
(check-true (inexact? 1.0))
(check-true (inexact? -1.0))
(check-true (inexact? 0.5))
(check-true (inexact? 3.14159))
(check-true (inexact? -3.14159))
(check-true (inexact? 1e10))
(check-true (inexact? 1.0+0.0i))        ;复数的实部/虚部是浮点数

;; 运算结果测试
(check-true (inexact? (+ 1.0 2.0)))     ;涉及不精确数的运算
(check-false (inexact? (+ 1 2)))        ;纯整数运算返回精确值
(check-true (inexact? (+ 1 2.0)))       ;混合运算返回不精确值
(check-false (inexact? (* 1/2 4)))      ;纯有理数运算返回精确值
(check-true (inexact? (* 0.5 4)))       ;涉及浮点的运算返回不精确值

;; 边界测试
(check-true (inexact? 1.7976931348623157e308))  ;最大浮点数
(check-true (inexact? 2.2250738585072014e-308)) ;最小正规化浮点数

;; 错误测试
(check-catch 'wrong-type-arg (inexact? "not a number"))
(check-catch 'wrong-type-arg (inexact? 'symbol))

(let1 zero-int 0
  (check-true (and (integer? zero-int) (zero? zero-int)))
) ;let1
(let1 zero-exact (- 1/2 1/2)
  (check-true (and (exact? zero-exact) (zero? zero-exact)))
) ;let1
(let1 zero-inexact 0.0
  (check-true (and (inexact? zero-inexact) (zero? zero-inexact)))
) ;let1

(check-false (zero? 1+1i))
(check-false (zero? #b11))

(check-catch 'wrong-type-arg (zero? #\A))
(check-catch 'wrong-type-arg (zero? #t))
(check-catch 'wrong-type-arg (zero? #f))

#|
zero?
判断一个数值是否为零。

语法
----
(zero? obj)

参数
----
obj : number?
任意数值类型。

返回值
-----
boolean?
如果 obj 是数值，当其为零时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是数值类型

|#

(check-true (zero? 0))
(check-true (zero? 0.0))
(check-true (zero? 0+0i))
(check-true (zero? 0/10))

(check-false (zero? 1))
(check-false (zero? 1.0))
(check-false (zero? -1))
(check-false (zero? -1.0))
(check-false (zero? +inf.0))
(check-false (zero? -inf.0))
(check-false (zero? +nan.0))
(check-false (zero? 1+i))

(check-catch 'wrong-type-arg (zero? #\A))
(check-catch 'wrong-type-arg (zero? #t))
(check-catch 'wrong-type-arg (zero? #f))
(check-catch 'wrong-type-arg (zero? "not-a-number"))
(check-catch 'wrong-type-arg (zero? 'symbol))
(check-catch 'wrong-type-arg (zero? '(1 2 3)))

#|
positive?
判断一个对象是否是正数。

语法
----
(positive? obj)

参数
----
obj : any
实数。

返回值
-----
boolean?
如果 obj 是实数类型，当其为正数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是实数类型（包括复数和非数值类型）

|#

(check-true (positive? 1))
(check-true (positive? 0.1))
(check-true (positive? 1/2))
(check-true (positive? +inf.0))
(check-true (positive? 1+0i))

(check-false (positive? 0))
(check-false (positive? -1))
(check-false (positive? -1.1))
(check-false (positive? -1/2))
(check-false (positive? -inf.0))
(check-false (positive? +nan.0))

(check-catch 'wrong-type-arg (positive? 1+1i))
(check-catch 'wrong-type-arg (positive? #\A))
(check-catch 'wrong-type-arg (positive? #t))
(check-catch 'wrong-type-arg (positive? "not-a-number"))
(check-catch 'wrong-type-arg (positive? 'symbol))
(check-catch 'wrong-type-arg (positive? '(1 2 3)))

#|
negative?
判断一个对象是否是负数。

语法
----
(negative? obj)

参数
----
obj : real?
实数。

返回值
-----
boolean?
如果 obj 是实数类型，当其为负数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是实数类型（包括复数和非数值类型）

|#


(check-true (negative? -1))
(check-true (negative? -0.1))
(check-true (negative? -1/2))
(check-true (negative? -inf.0))
(check-true (negative? -1+0i))

(check-false (negative? 0))
(check-false (negative? 1))
(check-false (negative? 1.1))
(check-false (negative? 1/2))
(check-false (negative? +inf.0))
(check-false (negative? -nan.0))

(check-catch 'wrong-type-arg (negative? -1-1i))
(check-catch 'wrong-type-arg (negative? #\A))
(check-catch 'wrong-type-arg (negative? #t))
(check-catch 'wrong-type-arg (negative? "not-a-number"))
(check-catch 'wrong-type-arg (negative? 'symbol))
(check-catch 'wrong-type-arg (negative? '(1 2 3)))

#|
odd?
判断一个整数是否是奇数。

语法
----
(odd? obj)

参数
----
obj : integer?
整数。

返回值
-----
boolean?
如果 obj 是整数类型，当其为奇数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是整数类型

|#

(check-true (odd? 1))
(check-false (odd? 0))

(check-catch 'wrong-type-arg (odd? 1+i))
(check-catch 'wrong-type-arg (odd? 1.0))
(check-catch 'wrong-type-arg (odd? 0.0))
(check-catch 'wrong-type-arg (odd? #\A))
(check-catch 'wrong-type-arg (odd? #t))
(check-catch 'wrong-type-arg (odd? #f))

#|
even?
判断一个整数是否是偶数。

语法
----
(even? obj)

参数
----
obj : integer?
整数。

返回值
-----
boolean?
如果 obj 是整数类型，当其为偶数时返回 #t，否则返回 #f。

错误
----
wrong-type-arg
如果参数不是整数类型

|#

(check-true (even? 0))
(check-false (even? 1))

(check-catch 'wrong-type-arg (even? 0.0))
(check-catch 'wrong-type-arg (even? 1.0))
(check-catch 'wrong-type-arg (even? 1+i))
(check-catch 'wrong-type-arg (even? #\A))
(check-catch 'wrong-type-arg (even? #t))
(check-catch 'wrong-type-arg (even? #f))

#|
max
返回所有给定实数的最大值。

语法
----
(max num ...)

参数
----
num : real?
任意个实数（大于等于1）。

返回值
------
real?
返回所给所有值的最大值。
如果存在NaN，返回NaN。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
type-error
如果存在任何参数不是实数，抛出错误。
wrong-number-of-args
如果没有提供参数，抛出错误。
|#

(check (max 7) => 7)  
(check (max 3.5) => 3.5) 
(check (max 1/3) => 1/3) 
(check (max +inf.0) => +inf.0) 
(check (max -inf.0) => -inf.0) 
(check (nan? (max +nan.0)) => #t) 


(check (max 7 3) => 7)  
(check (max 3.0 7.0) => 7.0)  
(check (max 3 7.0) => 7.0)  
(check (max 7.0 3) => 7.0)  
(check (max 1/2 1/3) => 1/2)  
(check (max 1/3 2/3) => 2/3)  
(check (max +inf.0 7) => +inf.0)  
(check (max 7 +inf.0) => +inf.0)  
(check (max -inf.0 7) => 7.0)  
(check (max 7 -inf.0) => 7.0)  
(check (nan? (max +nan.0 7)) => #t)  
(check (nan? (max 7 +nan.0)) => #t)  

(check (max 7 3 5) => 7)  
(check (max 3.0 7.0 2.0) => 7.0)  
(check (max 7 3.0 5) => 7.0)  
(check (max 1/2 1/3 2/3) => 2/3) 
(check (max +inf.0 7 3) => +inf.0)  
(check (max -inf.0 7 3) => 7.0) 
(check (nan? (max +nan.0 7 3)) => #t)  
(check (nan? (max 7 +nan.0 3)) => #t) 
(check (nan? (max +nan.0 +inf.0 -inf.0)) => #t) 

(check (max 7 3.0 5/4) => 7.0)  
(check (max 5.0 7/2 8) => 8.0)
(check (max +inf.0 7 3/4) => +inf.0)  
(check (max -inf.0 7 3.0) => 7.0) 
(check (nan? (max +nan.0 7.0 3)) => #t)  
(check (nan? (max 7/3 +nan.0 3)) => #t) 

(check-catch 'wrong-number-of-args (max))  
(check-catch 'type-error (max 'hello 7))  
(check-catch 'type-error (max "world" 7))  
(check-catch 'type-error (max #t 7))  
(check-catch 'type-error (max #f 7)) 
(check-catch 'type-error (max '(1 3 5) 7)) 
(check-catch 'type-error (max '() 7))  
(check-catch 'type-error (max 1+2i 2))  

#|
min
返回所有给定实数的最小值。

语法
----
(min num ...)

参数
----
num : real?
任意个实数（大于等于1）。

返回值
------
real?
返回所给所有值的最小值。
如果存在NaN，返回NaN。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
type-error
如果存在任何参数不是实数，抛出错误。
wrong-number-of-args
如果没有提供参数，抛出错误。
|#

(check (min 7) => 7)
(check (min 3.5) => 3.5)
(check (min 1/3) => 1/3)
(check (min +inf.0) => +inf.0)
(check (min -inf.0) => -inf.0)
(check (nan? (min +nan.0)) => #t)

(check (min 7 3) => 3)

(check (min 3.0 7.0) => 3.0)

(check (min 3 7.0) => 3.0)
(check (min 7.0 3) => 3.0)

(check (min 1/2 1/3) => 1/3)
(check (min 1/3 2/3) => 1/3)

(check (min +inf.0 7) => 7.0)
(check (min 7 +inf.0) => 7.0)
(check (min -inf.0 7) => -inf.0)
(check (min 7 -inf.0) => -inf.0)

(check (nan? (min +nan.0 7)) => #t)
(check (nan? (min 7 +nan.0)) => #t)

(check (min 7 3 5) => 3)

(check (min 3.0 7.0 2.0) => 2.0)

(check (min 7 3.0 5) => 3.0)

(check (min 1/2 1/3 2/3) => 1/3)

(check (min +inf.0 7 3) => 3.0)
(check (min -inf.0 7 3) => -inf.0)

(check (nan? (min +nan.0 7 3)) => #t)
(check (nan? (min 7 +nan.0 3)) => #t)
(check (nan? (min +nan.0 +inf.0 -inf.0)) => #t)

(check (min 7 3.0 15/4) => 3.0)  
(check (min 5.0 7/2 3) => 3.0)
(check (min +inf.0 7 39/4) => 7.0)  
(check (min -inf.0 7 3.0) => -inf.0) 
(check (nan? (min +nan.0 7.0 3)) => #t)  
(check (nan? (min 7/3 +nan.0 3)) => #t) 

(check-catch 'wrong-number-of-args (min))

(check-catch 'type-error (min 'hello 7))

(check-catch 'type-error (min "world" 7))

(check-catch 'type-error (min #t 7))
(check-catch 'type-error (min #f 7))

(check-catch 'type-error (min '(1 3 5) 7))
(check-catch 'type-error (min '() 7))

(check-catch 'type-error (min 1+2i 2))

#|
+
计算所有给定数字的和。

语法
----
(+ num ...)

参数
----
num : number?
任意个数字。

返回值
------
number?
如果没有参数，返回加法单位元 0
否则，返回其所有参数的和

错误
----
wrong-type-arg
如果存在任何参数不是数字，抛出错误。

|#

(check (+) => 0)
(check (+ 1) => 1)
(check (+ 1 2) => 3)
(check (+ 1 2 3) => 6)
(check (+ 1 2 3 4) => 10)

(check (+ 1.5 2.5) => 4.0)
(check (+ 0.1 0.2) => 0.30000000000000004)
(check (< (abs (- 3.3 (+ 1.1 2.2))) 1e-15) => #t)

(check (+ 1/2 1/2) => 1)
(check (+ 1/3 1/2) => 5/6)
(check (+ 1/3 1/4 1/5) => 47/60)

(check (+ 1+i 2+2i) => 3.0+3.0i)
(check (+ 3+2i 4-3i) => 7.0-1.0i)
(check (+ 1+i 1) => 2.0+1.0i)
(check (+ 1+i 1/2) => 1.5+1.0i)

(check (+ +inf.0 0.7) => +inf.0)
(check (+ -inf.0 7) => -inf.0)
(check (+ +inf.0 1+i) => +inf.0+1.0i)
(check (nan? (+ +nan.0 1)) => #t)
(check (nan? (+ +inf.0 -inf.0)) => #t)

(check (+ 1.0e308 1.0e308) => +inf.0)
(check (+ -1.0e308 -1.0e308) => -inf.0)
(check (+ #x7fffffffffffffff 1) => #x8000000000000000)

(check-catch 'wrong-type-arg (+ 'hello 7))
(check-catch 'wrong-type-arg (+ "world" 7))
(check-catch 'wrong-type-arg (+ #t 7))
(check-catch 'wrong-type-arg (+ '(1 3 5) 7))
(check-catch 'unbound-variable (+ 1+i 2i))

#|
-
计算所有给定数字的差。

语法
----
(- num ...)

参数
----
num : number?
一个或多个数字。

返回值
------
number?
如果只有一个参数，返回其加法逆元（相反数）
如果有多个参数，返回其所有参数左结合的差

错误
----
wrong-type-arg
如果存在任何参数不是数字，抛出错误。
wrong-number-of-args
如果没有提供参数，抛出错误。

|#

(check (- 5) => -5)
(check (- 2 1) => 1)
(check (- 7 2 1) => 4)
(check (- 10 1 2 3) => 4)

(check (- 1.5 0.5) => 1.0)
(check (< (abs(- 2.7 (- 6.98 2.5 1.78))) 1e-15) => #t)

(check (- 2/3 1/3) => 1/3)
(check (- 1/2 1/5 1/7) => 11/70)
(check (- 1 1/3) => 2/3)

(check (- 2+2i 1+i) => 1.0+1.0i)
(check (- 2+i 1) => 1.0+1.0i)
(check (- 1+i 1/2) => 0.5+1.0i)
(check (- 3+4i 0+2i 1+i) => 2.0+1.0i)

(check (- -inf.0 1) => -inf.0)
(check (- +inf.0 1) => +inf.0)
(check (- +inf.0 1+i) => +inf.0-1.0i)
(check (- 1 +inf.0) => -inf.0)
(check (- 1 -inf.0) => +inf.0)
(check (- 1+i +inf.0) => -inf.0+1.0i)
(check (nan? (- +nan.0 0.5)) => #t)
(check (nan? (- 1 2 -nan.0)) => #t)
(check (nan? (- +inf.0 +inf.0)) => #t)

(check-catch 'wrong-number-of-args (-))
(check-catch 'wrong-type-arg (- 'hello 7))
(check-catch 'wrong-type-arg (- "world" 7))
(check-catch 'wrong-type-arg (- #f 7))
(check-catch 'wrong-type-arg (- '(1 3 5) 7))
(check-catch 'unbound-variable (- 1+i 2i))

#|
*
乘法函数，支持整数、浮点数、有理数和复数的乘法运算。

语法
----
(* num ...)

参数
----
num : number?
任意个数字作为乘数。如果没有参数，则返回 1；如果只有一个参数，则返回该参数本身；
如果有多个参数，则依次相乘得到最终结果。

返回值
------
number?
如果没有参数，返回乘法单位元 1
如果只有一个参数，返回该参数本身
如果有多个参数，返回所有参数的乘积

说明
----
支持任意精确度和混合类型的乘法运算：
- 整数乘法：精确计算
- 浮点数乘法：可能出现精度误差
- 有理数乘法：保持精确分数
- 复数乘法：按复数乘法规则计算

错误
----
wrong-type-arg
如果存在任何参数不是数字类型，则抛出此错误
|#

(check (* 0 0) => 0)
(check (* 0 -1) => 0)
(check (* 0 1) => 0)
(check (* 0 2147483647) => 0)
(check (* 0 -2147483648) => 0)
(check (* 0 2147483648) => 0)
(check (* 0 -2147483649) => 0)
(check (* 0 9223372036854775807) => 0)
(check (* 0 -9223372036854775808) => 0)
(check (* 0 -9223372036854775809) => 0)

(check (* 1 0) => 0)
(check (* 1 -1) => -1)
(check (* 1 1) => 1)
(check (* 1 2147483647) => 2147483647)
(check (* 1 -2147483648) => -2147483648)
(check (* 1 2147483648) => 2147483648)
(check (* 1 -2147483649) => -2147483649)
(check (* 1 9223372036854775807) => 9223372036854775807)
(check (* 1 -9223372036854775808) => -9223372036854775808)
(check (* 1 9223372036854775807) => 9223372036854775807)

(check (* -1 0) => 0)
(check (* -1 -1) => 1)
(check (* -1 1) => -1)
(check (* -1 2147483647) => -2147483647)
(check (* -1 -2147483648) => 2147483648)
(check (* -1 2147483648) => -2147483648)
(check (* -1 -2147483649) => 2147483649)
(check (* -1 9223372036854775807) => -9223372036854775807)
(check (* -1 -9223372036854775808) => -9223372036854775808)
(check (* -1 9223372036854775807) => -9223372036854775807)

(check (* 2147483647 0) => 0)
(check (* 2147483647 -1) => -2147483647)
(check (* 2147483647 1) => 2147483647)
(check (* 2147483647 2147483647) => 4611686014132420609)
(check (* 2147483647 -2147483648) => -4611686016279904256)
(check (* 2147483647 2147483648) => 4611686016279904256)
(check (* 2147483647 -2147483649) => -4611686018427387903)
(check (* 2147483647 9223372036854775807) => 9223372034707292161)
(check (* 2147483647 -9223372036854775808) => -9223372036854775808)

(check (* -2147483648 0) => 0)
(check (* -2147483648 -1) => 2147483648)
(check (* -2147483648 1) => -2147483648)
(check (* -2147483648 2147483647) => -4611686016279904256)
(check (* -2147483648 -2147483648) => 4611686018427387904)
(check (* -2147483648 2147483648) => -4611686018427387904)
(check (* -2147483648 -2147483649) => 4611686020574871552)
(check (* -2147483648 9223372036854775807) => 2147483648)
(check (* -2147483648 -9223372036854775808) => 0)

(check (* 2147483648 0) => 0)
(check (* 2147483648 -1) => -2147483648)
(check (* 2147483648 1) => 2147483648)
(check (* 2147483648 2147483647) => 4611686016279904256)
(check (* 2147483648 -2147483648) => -4611686018427387904)
(check (* 2147483648 2147483648) => 4611686018427387904)
(check (* 2147483648 -2147483649) => -4611686020574871552)
(check (* 2147483648 9223372036854775807) => -2147483648)
(check (* 2147483648 -9223372036854775808) => 0)

(check (* -2147483649 0) => 0)
(check (* -2147483649 -1) => 2147483649)
(check (* -2147483649 1) => -2147483649)
(check (* -2147483649 2147483647) => -4611686018427387903)
(check (* -2147483649 -2147483648) => 4611686020574871552)
(check (* -2147483649 2147483648) => -4611686020574871552)
(check (* -2147483649 -2147483649) => 4611686022722355201)
(check (* -2147483649 9223372036854775807) => -9223372034707292159)
(check (* -2147483649 -9223372036854775808) => -9223372036854775808)

(check (* 9223372036854775807 0) => 0)
(check (* 9223372036854775807 -1) => -9223372036854775807)
(check (* 9223372036854775807 1) => 9223372036854775807)
(check (* 9223372036854775807 2147483647) => 9223372034707292161)
(check (* 9223372036854775807 -2147483648) => 2147483648)
(check (* 9223372036854775807 2147483648) => -2147483648)
(check (* 9223372036854775807 -2147483649) => -9223372034707292159)
(check (* 9223372036854775807 9223372036854775807) => 1)
(check (* 9223372036854775807 -9223372036854775808) => -9223372036854775808)

(check (* -9223372036854775808 0) => 0)
(check (* -9223372036854775808 -1) => -9223372036854775808)
(check (* -9223372036854775808 1) => -9223372036854775808)
(check (* -9223372036854775808 2147483647) => -9223372036854775808)
(check (* -9223372036854775808 -2147483648) => 0)
(check (* -9223372036854775808 2147483648) => 0)
(check (* -9223372036854775808 -2147483649) => -9223372036854775808)
(check (* -9223372036854775808 9223372036854775807) => -9223372036854775808)
(check (* -9223372036854775808 -9223372036854775808) => 0)

#|
abs
返回给定数值的绝对值。

语法
----
(abs num)

参数
----
num : real?
任意实数，包括整数、有理数或浮点数。

返回值
------
real?
输入数值的绝对值，保持输入值的类型精度。

说明
----
1. 对于非负输入返回输入值本身
2. 对于负输入返回其相反数
3. 对于有理数返回有理数绝对值
4. 对于浮点数返回浮点数绝对值
5. 零的绝对值是0

错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; abs 基本测试
(check (abs 0) => 0)
(check (abs 1) => 1)
(check (abs -1) => 1)
(check (abs 42) => 42)
(check (abs -42) => 42)
(check (abs 0.0) => 0.0)
(check (abs 1.5) => 1.5)
(check (abs -1.5) => 1.5)

;; 有理数测试
(check (abs 1/2) => 1/2)
(check (abs -1/2) => 1/2)
(check (abs 22/7) => 22/7)
(check (abs -22/7) => 22/7)

;; 边界测试
(check (abs 1000000000) => 1000000000)
(check (abs -1000000000) => 1000000000)

;; 零端点测试
(check (abs 0/1) => 0)
(check (abs 1/3) => 1/3)
(check (abs -1/3) => 1/3)

;; 错误处理测试
(check-catch 'wrong-type-arg (abs 1+2i))
(check-catch 'wrong-type-arg (abs "hello"))
(check-catch 'wrong-type-arg (abs 'symbol))
(check-catch 'wrong-number-of-args (abs))
(check-catch 'wrong-number-of-args (abs 1 2 3))


#|
floor
返回不大于给定数的最大整数。

语法
----
(floor num )

参数
----
num : real?
实数

返回值
------
返回不大于给定数的最大整数
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。
|#

(check (floor 1.1) => 1.0)
(check (floor 1) => 1)
(check (floor 1/2) => 0)
(check (floor 0) => 0)
(check (floor -1) => -1)
(check (floor -1.2) => -2.0)
(check-catch 'wrong-type-arg (floor 2+4i))
(check-catch 'wrong-type-arg (floor 'hello'))
(check-catch 'wrong-number-of-args (floor 4 5))
(check (s7-floor 1.1) => 1)
(check (s7-floor -1.2) => -2)

#|
ceiling
返回不小于给定数的最小整数。

语法
----
(ceiling num )

参数
----
num : real?
实数

返回值
------
返回不小于给定数的最小整数
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。
|#

(check (ceiling 1.1) => 2.0)
(check (ceiling 1) => 1)
(check (ceiling 1/2) => 1)
(check (ceiling 0) => 0)
(check (ceiling -1) => -1)
(check (ceiling -1.2) => -1.0)
(check-catch 'wrong-type-arg (ceiling 2+4i))
(check-catch 'wrong-type-arg (ceiling 'hello'))
(check-catch 'wrong-number-of-args (ceiling 4 5))

(check (s7-ceiling 1.1) => 2)
(check (s7-ceiling -1.2) => -1)

#|
truncate
返回在靠近零的方向上最靠近给定数的整数。

语法
----
(truncate num )

参数
----
num : real?
实数

返回值
------
返回在靠近零的方向上最靠近给定数的整数，即正数向下取整，负数向上取整
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的

错误
----
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。
|#

(check (truncate 1.1) => 1.0)
(check (truncate 1) => 1)
(check (truncate 1/2) => 0)
(check (truncate 0) => 0)
(check (truncate -1) => -1)
(check (truncate -1.2) => -1.0)
(check-catch 'wrong-type-arg (truncate 2+4i))
(check-catch 'wrong-type-arg (truncate 'hello'))
(check-catch 'wrong-number-of-args (truncate 4 5))

(check (s7-truncate 1.1) => 1)
(check (s7-truncate -1.2) => -1)

#|
round
round用于返回最接近给定数的整数。

语法
----
(round num)

参数
----
num :real?
实数值，精确的或非精确的

返回值
------
实数? -> (or (integer? integer)
             (real? real-with-trailing-decimal))
返回最接近给定数的整数，如果两个整数同样接近，则取往远离零的方向取整。
如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的。

说明
----
1. 当小数部分等于0.5时，round按照IEEE 754标准（向偶数取整）
   (例如：round(1.5) => 2, round(0.5) => 0, round(2.5) => 2, round(-1.5) => -2)
2. 对于精确值(整数、有理数)返回精确值
   (例如：round(1/3) => 0, round(3/4) => 1)
3. 对于非精确值(浮点数、复数)返回非精确值
   (例如：round(1.1) => 1.0, round(3.9) => 4.0)
4. 对于实部为复数的数值，round会分别对实部和虚部四舍五入

错误处理
--------
wrong-type-arg
如果参数不是实数，抛出错误。
wrong-number-of-args
如果参数数量不为一，抛出错误。

|#

(check (round 1.1) => 1.0)
(check (round 1.5) => 2.0)
(check (round 1) => 1)
(check (round 1/2) => 0)
(check (round 0) => 0)
(check (round -1) => -1)
(check (round -1.2) => -1.0)
(check (round -1.5) => -2.0)

;; 测试四舍五入到最近的整数
(check (round 0) => 0)
(check (round 0.4) => 0.0)
(check (round 0.5) => 0.0)      ; 0.5 -> 0 (IEEE 754向偶数取整)
(check (round 0.6) => 1.0)
(check (round 1.4) => 1.0)
(check (round 1.5) => 2.0)      ; 1.5 -> 2
(check (round 1.6) => 2.0)
(check (round 2.5) => 2.0)      ; 2.5 -> 2 (IEEE 754向偶数取整)
(check (round 3.5) => 4.0)      ; 3.5 -> 4

;; 测试负数情况
(check (round -0.4) => 0.0)
(check (round -0.5) => -0.0)    ; -0.5 -> -0.0 (IEEE 754向偶数取整)
(check (round -0.6) => -1.0)
(check (round -1.4) => -1.0)
(check (round -1.5) => -2.0)    ; -1.5 -> -2
(check (round -2.5) => -2.0)    ; -2.5 -> -2 (IEEE 754向偶数取整)
(check (round -3.5) => -4.0)    ; -3.5 -> -4

;; 测试整数边界
(check (round 2147483647) => 2147483647)
(check (round -2147483648) => -2147483648)

;; 测试有理数情况
(check (round 1/3) => 0)
(check (round 2/3) => 1)
(check (round 3/4) => 1)
(check (round -1/3) => 0)
(check (round -2/3) => -1)
(check (round -3/4) => -1)

;; 测试错误情况
(check-catch 'wrong-type-arg (round "not a number"))
(check-catch 'wrong-type-arg (round 'symbol))
(check-catch 'wrong-type-arg (round 1+2i))  ; 复数目前不支持
(check-catch 'wrong-number-of-args (round))
(check-catch 'wrong-number-of-args (round 1 2))

#|
floor-quotient
用于计算两个数的地板除法，返回向负无穷取整的商。

语法
----
(floor-quotient dividend divisor)

参数
----
dividend : number? - 被除数
divisor : number? - 除数，不能为零

返回值
------
number?
返回一个整数，表示向负无穷方向取整的商。

错误
----
division-by-zero
当除数为零时抛出错误。
wrong-type-arg
当参数不是数字时抛出错误。
|#

(check (floor-quotient 11 2) => 5)
(check (floor-quotient 11 -2) => -6)
(check (floor-quotient -11 2) => -6)
(check (floor-quotient -11 -2) => 5)

(check (floor-quotient 10 2) => 5)
(check (floor-quotient 10 -2) => -5)
(check (floor-quotient -10 2) => -5)
(check (floor-quotient -10 -2) => 5)

(check-catch 'division-by-zero (floor-quotient 11 0))
(check-catch 'division-by-zero (floor-quotient 0 0))
(check-catch 'wrong-type-arg (floor-quotient 1+i 2))

(check (floor-quotient 0 2) => 0)
(check (floor-quotient 0 -2) => 0)

(check (receive (q r) (floor/ 11 3) q) => 3)
(check (receive (q r) (floor/ 11 3) r) => 2)
(check (receive (q r) (floor/ 11 -3) q) => -4)
(check (receive (q r) (floor/ 11 -3) r) => -1)
(check (receive (q r) (floor/ -11 3) q) => -4)
(check (receive (q r) (floor/ -11 3) r) => 1)
(check (receive (q r) (floor/ -11 -3) q) => 3)
(check (receive (q r) (floor/ -11 -3) r) => -2)

(check (receive (q r) (floor/ 10 2) q) => 5)
(check (receive (q r) (floor/ 10 2) r) => 0)
(check (receive (q r) (floor/ 10 -2) q) => -5)
(check (receive (q r) (floor/ 10 -2) r) => 0)
(check (receive (q r) (floor/ -10 2) q) => -5)
(check (receive (q r) (floor/ -10 2) r) => 0)
(check (receive (q r) (floor/ -10 -2) q) => 5)
(check (receive (q r) (floor/ -10 -2) r) => 0)

(check (receive (q r) (floor/ 15 4) q) => 3)
(check (receive (q r) (floor/ 15 4) r) => 3)
(check (receive (q r) (floor/ 15 -4) q) => -4)
(check (receive (q r) (floor/ 15 -4) r) => -1)
(check (receive (q r) (floor/ -15 4) q) => -4)
(check (receive (q r) (floor/ -15 4) r) => 1)
(check (receive (q r) (floor/ -15 -4) q) => 3)
(check (receive (q r) (floor/ -15 -4) r) => -3)

(check (receive (q r) (floor/ 1 3) q) => 0)
(check (receive (q r) (floor/ 1 3) r) => 1)
(check (receive (q r) (floor/ 0 5) q) => 0)
(check (receive (q r) (floor/ 0 5) r) => 0)

(check-catch 'division-by-zero (floor/ 11 0))
(check-catch 'division-by-zero (floor/ 0 0))
(check-catch 'wrong-type-arg (floor/ 1+i 2))
(check-catch 'wrong-type-arg (floor/ 5 #t))

#|
truncate/
测试截断除法函数的各种情况
|#

(check (receive (q r) (truncate/ 11 3) q) => 3)
(check (receive (q r) (truncate/ 11 3) r) => 2)
(check (receive (q r) (truncate/ 11 -3) q) => -3)
(check (receive (q r) (truncate/ 11 -3) r) => 2)
(check (receive (q r) (truncate/ -11 3) q) => -3)
(check (receive (q r) (truncate/ -11 3) r) => -2)
(check (receive (q r) (truncate/ -11 -3) q) => 3)
(check (receive (q r) (truncate/ -11 -3) r) => -2)

(check (receive (q r) (truncate/ 10 2) q) => 5)
(check (receive (q r) (truncate/ 10 2) r) => 0)
(check (receive (q r) (truncate/ 10 -2) q) => -5)
(check (receive (q r) (truncate/ 10 -2) r) => 0)
(check (receive (q r) (truncate/ -10 2) q) => -5)
(check (receive (q r) (truncate/ -10 2) r) => 0)
(check (receive (q r) (truncate/ -10 -2) q) => 5)
(check (receive (q r) (truncate/ -10 -2) r) => 0)

(check (receive (q r) (truncate/ 15 4) q) => 3)
(check (receive (q r) (truncate/ 15 4) r) => 3)
(check (receive (q r) (truncate/ 15 -4) q) => -3)
(check (receive (q r) (truncate/ 15 -4) r) => 3)
(check (receive (q r) (truncate/ -15 4) q) => -3)
(check (receive (q r) (truncate/ -15 4) r) => -3)
(check (receive (q r) (truncate/ -15 -4) q) => 3)
(check (receive (q r) (truncate/ -15 -4) r) => -3)

(check (receive (q r) (truncate/ 1 3) q) => 0)
(check (receive (q r) (truncate/ 1 3) r) => 1)
(check (receive (q r) (truncate/ 0 5) q) => 0)
(check (receive (q r) (truncate/ 0 5) r) => 0)

(check-catch 'division-by-zero (truncate/ 11 0))
(check-catch 'division-by-zero (truncate/ 0 0))
(check-catch 'wrong-type-arg (truncate/ 1+i 2))
(check-catch 'wrong-type-arg (truncate/ 5 #t))

#|
quotient
用于计算两个数的精确除法商（向零取整）。

语法
----
(quotient dividend divisor)

参数
----
dividend : real? - 被除数
divisor : real? - 除数，不能为零

返回值
------
integer?
返回一个整数，表示向零方向取整的商。

与floor-quotient的区别
-------------
quotient与floor-quotient的主要区别在于对负数除法的处理：
- quotient：向零取整（截断除法），如(quotient -11 2) => -5
- floor-quotient：向负无穷取整，如(floor-quotient -11 2) => -6

错误
----
division-by-zero
当除数为零时抛出错误。
wrong-type-arg
当参数不是数字时抛出错误。
|#

(check (quotient 11 2) => 5)
(check (quotient 11 -2) => -5)
(check (quotient -11 2) => -5)
(check (quotient -11 -2) => 5)

(check (quotient 10 3) => 3)
(check (quotient 10 -3) => -3)
(check (quotient -10 3) => -3)
(check (quotient -10 -3) => 3)

(check (quotient 0 5) => 0)
(check (quotient 0 -5) => 0)
(check (quotient 15 5) => 3)
(check (quotient -15 5) => -3)

(check (quotient 7 7) => 1)
(check (quotient 100 10) => 10)
(check (quotient 1 1) => 1)
(check (quotient -1 1) => -1)

(check (quotient 17 5) => 3)
(check (quotient -17 5) => -3)
(check (quotient 17 -5) => -3)
(check (quotient -17 -5) => 3)

(check-catch 'division-by-zero (quotient 11 0))
(check-catch 'division-by-zero (quotient 0 0))
(check (quotient 10.5 3.0) => 3)
(check (quotient 10.5 -3.0) => -3)
(check (quotient -10.5 3.0) => -3)
(check (quotient -10.5 -3.0) => 3)
(check-catch 'wrong-type-arg (quotient 1+i 2))
(check-catch 'wrong-type-arg (quotient 'hello 2))
(check-catch 'wrong-number-of-args (quotient 10))
(check-catch 'wrong-number-of-args (quotient 5 3 2))

#|
remainder
计算两个实数相除的余数。

语法
----
(remainder dividend divisor)

参数
----
dividend : real?
被除数

divisor : real?
除数，不能为零

返回值
------
real?
返回被除数除以除数的余数
当参数中存在不精确数时，返回不精确数。否则，返回一个精确数。

错误处理
--------
division-by-zero
当除数为零时抛出错误。
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为二时抛出错误。
|#

(check (remainder 5 2) => 1)
(check (remainder -5 2) => -1)
(check (remainder 5 -2) => 1)
(check (remainder -5 -2) => -1)
(check (remainder 10 3) => 1)
(check (remainder -10 3) => -1)
(check (remainder 0 5) => 0)
(check (remainder 15 5) => 0)
(check (remainder 16 5) => 1)
(check (remainder 11/2 3) => 5/2)

(check-catch 'division-by-zero (remainder 5 0))
(check-catch 'wrong-type-arg (remainder 5 "hello"))
(check-catch 'wrong-type-arg (remainder 2+8i 5))
(check-catch 'wrong-number-of-args (remainder 5))
(check-catch 'wrong-number-of-args (remainder 5 2 3))

#|
modulo
计算实数的取模运算。

语法
----
(modulo dividend divisor)

参数
----
dividend : real? - 被除数
divisor : real? - 除数，不能为零

返回值
------
real?
返回 dividend 除以 divisor 的余数。

错误
----
type-error
当任一参数不是实数类型时抛出错误。包括复数（如 1+2i）、字符串、符号等其他类型。

division-by-zero  
当除数 divisor 为零时抛出错误。

wrong-number-of-args
当参数数量不为两个时抛出错误。
|#

(check (modulo 13 4) => 1)
(check (modulo -13 4) => 3)    
(check (modulo 13 -4) => -3)   
(check (modulo -13 -4) => -1)  
(check (modulo 0 5) => 0)    
(check (modulo 0 -5) => 0)    

(check (modulo 13 4.0) => 1.0)     
(check (modulo -13.0 4) => 3.0)    
(check (modulo 13.0 -4.0) => -3.0) 
(check (modulo 1000000 7) => 1)    
(check (modulo 1 1) => 0)
(check (modulo 5 5) => 0)
(check (modulo -1 5) => 4)
(check (modulo -5 5) => 0)
(check (modulo 20 7) => 6)
(check (modulo -20 7) => 1)
(check (modulo 20 -7) => -1)


(check-catch 'type-error (modulo 1+i 2))
(check-catch 'type-error (modulo 'hello 2))
(check-catch 'wrong-number-of-args (modulo 5))
(check-catch 'wrong-number-of-args (modulo 5 3 2))
(check-catch 'division-by-zero (modulo 1 0))

(check (floor-remainder 13 4) => 1)
(check (floor-remainder -13 4) => 3)    
(check (floor-remainder 13 -4) => -3)   
(check (floor-remainder -13 -4) => -1)  
(check (floor-remainder 0 5) => 0)    
(check (floor-remainder 0 -5) => 0)    

(check (floor-remainder 13 4.0) => 1.0)     
(check (floor-remainder -13.0 4) => 3.0)    
(check (floor-remainder 13.0 -4.0) => -3.0) 
(check (floor-remainder 1000000 7) => 1)    
(check (floor-remainder 1 1) => 0)
(check (floor-remainder 5 5) => 0)
(check (floor-remainder -1 5) => 4)
(check (floor-remainder -5 5) => 0)
(check (floor-remainder 20 7) => 6)
(check (floor-remainder -20 7) => 1)
(check (floor-remainder 20 -7) => -1)

(check-catch 'type-error (floor-remainder 1+i 2))
(check-catch 'type-error (floor-remainder 'hello 2))
(check-catch 'wrong-number-of-args (floor-remainder 5))
(check-catch 'wrong-number-of-args (floor-remainder 5 3 2))
(check-catch 'division-by-zero (floor-remainder 1 0))

#|
gcd
用于计算给定整数的最大公约数。

语法
----
(gcd integer ...)

参数
----
integer : integer? - 整数。接受零个、一个或多个参数。

返回值
------
integer?
返回所有参数的最大公约数。无参数时返回0，单参数时返回该参数的绝对值。

特殊规则
--------
- 无参数时返回0
- 参数中包含0时，忽略0值
- 负数会被取绝对值处理
- 多个参数按顺序计算最大值公约数

错误处理
----
wrong-type-arg
当参数不是整数时抛出错误。
|#

(check (gcd) => 0)
(check (gcd 0) => 0)
(check (gcd 1) => 1)
(check (gcd 2) => 2)
(check (gcd -1) => 1)

(check (gcd 0 1) => 1)
(check (gcd 1 0) => 1)
(check (gcd 1 2) => 1)
(check (gcd 1 10) => 1)
(check (gcd 2 10) => 2)
(check (gcd -2 10) => 2)

(check (gcd 2 3 4) => 1)
(check (gcd 2 4 8) => 2)
(check (gcd -2 4 8) => 2)
(check (gcd 15 20 25) => 5)
(check (gcd 6 9 12 15) => 3)
(check (gcd 0 4 6) => 2)
(check (gcd 1 2 3 4 5) => 1)
(check (gcd 12 18) => 6)
(check (gcd 18 12) => 6)
(check (gcd 21 35) => 7)
(check (gcd 0 5) => 5)
(check (gcd 15 0) => 15)
(check (gcd -6 8) => 2)
(check (gcd 12 -9) => 3)

(check-catch 'wrong-type-arg (gcd 1.5))
(check-catch 'wrong-type-arg (gcd 2.3))
(check-catch 'wrong-type-arg (gcd 1+i))
(check-catch 'wrong-type-arg (gcd 'hello))
(check-catch 'wrong-type-arg (gcd 1 2+i 3))
(check-catch 'wrong-type-arg (gcd 1.5 2.5))

#|
lcm
计算给定有理数的最小公倍数。

语法
----
(lcm reals ...)

参数
----
reals : real?
实数。接受零个、一个或多个参数。

返回值
------
返回最小公倍数。
无参数时返回1，单参数时返回该参数本身的绝对值，参数中包含0时返回0。
如果参数中含有不精确值，返回值也是不精确的。

错误处理
--------
type-error
当参数不是实数时抛出错误。
|#

;; 基本测试
(check (lcm) => 1)
(check (lcm 1) => 1)
(check (lcm 0) => 0)
(check (lcm -1) => 1)

(check (lcm 2 3) => 6)
(check (lcm 4 6) => 12)
(check (lcm 12 18) => 36)
(check (lcm -6 8) => 24)
(check (lcm 0 5) => 0)

(check (lcm 2 4 5) => 20)
(check (lcm 6 8 9) => 72)

(check (lcm 5/2 4) => 20)  
(check (lcm 32.0 -36.0) => 288.0)
(check (lcm 32.0 -36) => 288.0)
(check-catch 'type-error (lcm 1+2i))


#|
numerator
返回有理数的分子部分。

语法
----
(numerator q)

参数
----
q : rational?
有理数。

返回值
------
integer?
返回有理数的分子部分。
对于整数，分子是整数本身；对于有理数a/b，返回a。

错误处理
--------
wrong-type-arg
当参数不是有理数时抛出错误。
|#

;; numerator测试
(check (numerator 1/2) => 1)
(check (numerator 4/5) => 4)
(check (numerator -3/7) => -3)
(check (numerator 5) => 5)
(check (numerator 0) => 0)
(check (numerator (inexact->exact 2.5)) => 5)

;; 补充numerator测试
(check (numerator 42) => 42)
(check (numerator -42) => -42)
(check (numerator 1/3) => 1)
(check (numerator 10/5) => 2)
(check (numerator -4/8) => -1)
(check (numerator 0) => 0)

#|
denominator
返回有理数的分母部分。

语法
----
(denominator q)

参数
----
q : rational?
有理数。

返回值
------
integer?
返回有理数的分母部分。
对于整数，分母是1；对于有理数a/b，返回b，b总是正整数。

错误处理
--------
wrong-type-arg
当参数不是有理数时抛出错误。
|#

;; denominator测试
(check (denominator 1/2) => 2)
(check (denominator 4/5) => 5)
(check (denominator -3/7) => 7)
(check (denominator 5) => 1)
(check (denominator 0) => 1)
(check (denominator (inexact->exact 2.5)) => 2)

;; 补充denominator测试  
(check (denominator 42) => 1)
(check (denominator -42) => 1)
(check (denominator 1/3) => 3)
(check (denominator 10/5) => 1)
(check (denominator -4/8) => 2)
(check (denominator (inexact->exact 5.5)) => 2)
(check (denominator (inexact->exact 0.25)) => 4)

#|
rationalize
将给定的实数简化为一个具有较小分母的近似有理数。

语法
----
(rationalize x [within])

参数
----
x : real?
要简化的实数

within : real?(可选)
容差范围，表示结果与原始值之间的最大允许误差，输入0或仅有一个参数时within默认为0.000000000001。

返回值
------
real?
返回满足|(result - x)| <= within且分母最小的有理数

错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为2个时抛出错误。
|#

(check (rationalize 0.5 0.1) => 1/2)
(check (rationalize 0.33 0.01) => 1/3)
(check (rationalize 0.333 0.02) => 1/3)
(check (rationalize 3.14159265359 0.01) => 22/7)
(check (rationalize 3.14159265359 0.001) => 201/64)
(check (rationalize 0.0 0.1) => 0)
(check (rationalize 1.0 0.0) => 1)
(check (rationalize 0.999 0.001) => 1)
(check (rationalize -0.5 0.1) => -1/2)

(check (rationalize 2.71828 0.0001) => 193/71)
(check (rationalize 1.4142 0.001) => 41/29)
(check (rationalize 2/3 0.05) => 2/3)

(check-catch 'wrong-type-arg (rationalize "hello" 0.1))
(check-catch 'wrong-number-of-args (rationalize 3.14 0.01 0.02))

#|
square
计算给定数值的平方。

语法
----
(square x)

参数
----
x : number?
数值。支持整数、有理数、浮点数等各种数值类型。

返回值
------
返回x的平方值，保持与输入相同的数值类型精度。
对于整数，返回精确的平方值；对于浮点数，返回浮点数平方值。

错误处理
--------
wrong-type-arg
当参数不是数值时抛出错误。
|#

;; square测试
(check (square 2) => 4)
(check (square 0) => 0)
(check (square -2) => 4)
(check (square 5) => 25)
(check (square -5) => 25)
(check (square 1/2) => 1/4)
(check (square -1/3) => 1/9)
(check (square 2.5) => 6.25)
(check (square 0.0) => 0.0)
(check (square 10) => 100)
(check (square 1+2i) => -3+4i)
(check-catch 'wrong-type-arg (square "a"))


;; 补充square边界测试
(check (square 1) => 1)
(check (square -1) => 1)
(check (square 1000) => 1000000)
(check (square 1/100) => 1/10000)
(check (square 0.001) => 0.000001)

#|
/
除法函数，支持整数、浮点数、有理数和复数的除法运算。

语法
----
(/ num ...)

参数
----
num : number?
任意个数字作为除数。如果没有参数，抛出错误；如果只有一个参数，则返回其倒数；如果有多个参数，则从第二个参数开始依次除第一个参数。

返回值
------
number?
如果没有参数，抛出错误；如果只有一个参数，返回其倒数；如果有多个参数，返回其左结合的商。

说明
----
支持任意精确度和混合类型的除法运算：
- 整数除法：精确计算，如果没有模除则保持精确
- 浮点数除法：可能出现精度误差
- 有理数除法：保持精确分数
- 复数除法：按复数除法规则计算

错误
----
wrong-type-arg
如果存在任何参数不是数字类型，则抛出此错误
division-by-zero
除数为零时抛出此错误
wrong-number-of-args
提供的参数个数与函数定义时所需的参数个数不匹配
|#

(check (/ 5) => 1/5)
(check (/ 1) => 1)
(check (/ -1) => -1)
(check (/ 2.5) => 0.4)
(check (/ 0.1) => 10.0)
(check (/ 1/2) => 2)
(check (/ 4/3) => 3/4)
(check (/ 10 2) => 5)
(check (/ 3 4) => 3/4)
(check (< (abs (- (/ 1.2 0.3) 4.0)) 1e-15) => #t)
(check (/ 2/3 1/3) => 2)
(check (/ 6 4 2) => 3/4)
(check (/ 6 2 3) => 1)
(check (/ 120 2 3 4 5) => 1)

(check (/ 10 3) => 10/3)
(check (/ 1/2 1/3) => 3/2)
(check (/ 4/5 2/3) => 6/5)

(check (/ 1 1) => 1)
(check (/ 1+0i 1+0i) => 1.0)

(check (/ -10 5) => -2)
(check (/ 10 -5) => -2)
(check (/ -10 -5) => 2)

(check (/ 5.0 2.0) => 2.5)
(check (/ 1.0 3.0) => 0.3333333333333333)
(check (/ 1/2 0.5) => 1.0)
(check (/ 4/2 2) => 1)

(check-catch 'division-by-zero (/ 5 0))
(check-catch 'division-by-zero (/ 1 0 2))
(check-catch 'division-by-zero (/ 0))
(check-catch 'wrong-type-arg (/ 'hello 7))
(check-catch 'wrong-type-arg (/ "world" 7))
(check-catch 'wrong-type-arg (/ 5 #t))
(check-catch 'wrong-number-of-args (/))

#|
exact-integer-sqrt
计算给定非负精确整数的精确平方根。

语法
----
(exact-integer-sqrt n)

参数
----
n : exact?
n是确切的非负整数。

返回值
------
values
返回两个值：
1. 整数r：满足r² ≤ n的最大整数
2. 整数remainder：n - r²，始终为非负

说明
----
该函数专为精确计算设计，要求参数必须是非负的准确整数。
对于完全平方数，remainder将为0；非完全平方数返回最大的整数根和余量。

错误处理
--------
type-error
当参数不是准确的整数时抛出错误。
value-error
当参数是负数时抛出错误。
|#

(check (list (exact-integer-sqrt 9)) => (list 3 0))
(check (list (exact-integer-sqrt 5)) => (list 2 1))
(check (list (exact-integer-sqrt 0)) => (list 0 0))
(check (list (exact-integer-sqrt 1)) => (list 1 0))
(check (list (exact-integer-sqrt 4)) => (list 2 0))
(check (list (exact-integer-sqrt 16)) => (list 4 0))
(check (list (exact-integer-sqrt 2)) => (list 1 1))
(check (list (exact-integer-sqrt 3)) => (list 1 2))
(check (list (exact-integer-sqrt 8)) => (list 2 4))
(check (list (exact-integer-sqrt 25)) => (list 5 0))
(check (list (exact-integer-sqrt 100)) => (list 10 0))
(check (list (exact-integer-sqrt 1000)) => (list 31 39))
(check (list (exact-integer-sqrt 1000000)) => (list 1000 0))
(check-catch 'type-error (exact-integer-sqrt "a"))
(check-catch 'value-error (exact-integer-sqrt -1))
(check-catch 'type-error (exact-integer-sqrt 1.1))
(check-catch 'type-error (exact-integer-sqrt 1+i)) 

#|
number->string
将数值转换为字符串表示。

语法
----
(number->string num)
(number->string num radix)

参数
----
num : number?
要转换为字符串的数值，支持整数、实数、有理数、复数等各种数值类型。

radix : exact?
可选参数，指定转换的进制。必须是精确的整数，范围在2到16之间（包含2和16）。
当不指定时，默认为10进制。

返回值
------
string?
返回给定数值的字符串表示形式。

说明
----
1. 对于整数，返回整数字符串表示
2. 对于实数，返回小数格式的字符串
3. 对于有理数，返回"分子/分母"格式的字符串
4. 对于复数，返回"实部+虚部i"格式的字符串
5. 指定进制时，返回指定进制的字符串表示（仅适用于有理的实数部分）

错误处理
--------
wrong-type-arg
当参数不是数值或进制不是精确的整数时抛出错误。
out-of-range
当进制不在2到16范围内时抛出错误。
wrong-number-of-args
当参数数量不为1或2时抛出错误。
|#

;; 基本整数转换测试
(check (number->string 123) => "123")
(check (number->string 0) => "0")
(check (number->string -456) => "-456")
(check (number->string 2147483647) => "2147483647")
(check (number->string -2147483648) => "-2147483648")

;; 基本进制转换测试
(check (number->string 123 2) => "1111011")
(check (number->string 123 8) => "173")
(check (number->string 255 16) => "ff")
(check (number->string 255 10) => "255")

;; 有理数转换测试
(check (number->string 1/2) => "1/2")
(check (number->string -1/3) => "-1/3")
(check (number->string 22/7) => "22/7")
(check (number->string 0/1) => "0")

;; 有理数进制转换测试
(check (number->string 1/2 2) => "1/10")
(check (number->string 3/4 2) => "11/100")
(check (number->string 1/3 16) => "1/3")

;; 浮点数转换测试
(check (number->string 123.456) => "123.456")
(check (number->string 0.0) => "0.0")
(check (number->string -0.123) => "-0.123")
(check (number->string 1.23e10) => "1.23e+10")
(check (number->string 1.23e-3) => "0.00123")

;; 复数转换测试
(check (number->string 1+2i) => "1.0+2.0i")
(check (number->string 0+2i) => "0.0+2.0i")
(check (number->string -3+4i) => "-3.0+4.0i")
(check (number->string 1.5-2.5i) => "1.5-2.5i")
(check (number->string 0+1i) => "0.0+1.0i")
(check (number->string 0+0i) => "0.0")
(check (number->string 1.0+0.0i) => "1.0")

;; 边界测试
(check (number->string 1 2) => "1")
(check (number->string 0 16) => "0")
(check (number->string -128 16) => "-80")
(check (number->string 1023 2) => "1111111111")

;; 错误处理测试
(check-catch 'wrong-type-arg (number->string 'not-a-number))
(check-catch 'wrong-type-arg (number->string 123 'not-a-number))
(check-catch 'out-of-range (number->string 123 1)) 
(check-catch 'out-of-range (number->string 123 37))
(check-catch 'wrong-type-arg (number->string 123 3.5))
(check-catch 'wrong-number-of-args (number->string))
(check-catch 'wrong-number-of-args (number->string 123 2 3))

#|
string->number
将字符串解析为数值。根据R7RS规范，支持多种数值格式的解析。

语法
----
(string->number str)
(string->number str radix)

参数
----
str : string?
要解析为数值的字符串。支持整数、实数、有理数、复数、浮点数科学计数法等格式。
radix : exact-integer?
可选参数，指定解析的进制。必须是精确的整数，范围在2到16之间（包含2和16）。
当不指定时，默认为10进制。

返回值
------
number? | #f
如果字符串可以成功解析为数值，则返回对应的数值，否则返回#f。

说明
----
string->number 支持解析以下格式：
- 整数："123", "-456"
- 浮点数："123.456", "-0.123"
- 科学计数法："1.23e10", "1.23e-3"
- 有理数："1/2", "-22/7"
- 复数："1+2i", "3.14-2.71i"
- 不同进制：二进制"1010", 八进制"755", 十六进制"FF"

错误情况
-------
当radix参数超出有效范围（2-16）时，行为未定义（S7中返回#f）。
|#

;; 基本整数解析测试
(check (string->number "123") => 123)
(check (string->number "0") => 0)
(check (string->number "-456") => -456)
(check (string->number "2147483647") => 2147483647)
(check (string->number "-2147483648") => -2147483648)

;; 基本进制解析测试
(check (string->number "1111011" 2) => 123)
(check (string->number "173" 8) => 123)
(check (string->number "ff" 16) => 255)
(check (string->number "255" 10) => 255)
(check (string->number "10" 2) => 2)
(check (string->number "77" 8) => 63)

;; 浮点数解析测试
(check (string->number "123.456") => 123.456)
(check (string->number "0.0") => 0.0)
(check (string->number "-0.123") => -0.123)
(check (string->number "1.23e10") => 1.23e10)
(check-true (< (abs (- (string->number "1.23e-3") 0.00123)) 1e-10))
(check (string->number "1e5") => 100000.0)
(check (string->number "1e-5") => 0.00001)

;; 有理数解析测试
(check (string->number "1/2") => 1/2)
(check (string->number "-1/3") => -1/3)
(check (string->number "22/7") => 22/7)
(check (string->number "0/1") => 0)
(check (string->number "-22/7") => -22/7)

;; 有理数进制解析测试
(check (string->number "1/10" 2) => 1/2)
(check (string->number "11/100" 2) => 3/4)

;; 复数解析测试
(check (string->number "1+2i") => 1+2i)
(check (string->number "0+2i") => 0+2i)
(check (string->number "-3+4i") => -3+4i)
(check (string->number "3.14-2.71i") => 3.14-2.71i)
(check (string->number "0+1i") => 0+1i)
(check (string->number "0+0i") => 0.0)
(check (string->number "1.0+0.0i") => 1.0)
(check (string->number "-2.5-1.5i") => -2.5-1.5i)

;; 无效字符串解析测试（应返回#f）
(check (string->number "abc") => #f)
(check (string->number "123abc") => #f)
(check (string->number "abc123") => #f)
(check (string->number "1.2.3") => #f)
(check (string->number "1/2/3") => #f)
(check (string->number "1+i+i") => #f)
(check (string->number "") => #f)
(check (string->number "   ") => #f)
(check (string->number "1 2") => #f)

;; 边界测试
(check (string->number "1" 2) => 1)
(check (string->number "0" 16) => 0)
(check (string->number "-80" 16) => -128)
(check (string->number "1111111111" 2) => 1023)

;; 十六进制测试
(check (string->number "FF" 16) => 255)
(check (string->number "-FF" 16) => -255)
(check (string->number "A" 16) => 10)
(check (string->number "a" 16) => 10)

;; 错误处理测试（无效进制）
(check-catch 'out-of-range (string->number "123" 1))
(check-catch 'out-of-range (string->number "123" 17))
(check-catch 'out-of-range (string->number "123" 0))
(check-catch 'out-of-range (string->number "123" -1))

;; 错误参数测试
(check-catch 'wrong-type-arg (string->number 123))
(check-catch 'wrong-type-arg (string->number 'symbol))
(check-catch 'wrong-type-arg (string->number #t))
(check-catch 'wrong-type-arg (string->number "123" 'not-a-number))
(check-catch 'wrong-type-arg (string->number "123" 3.5))
(check-catch 'wrong-number-of-args (string->number))
(check-catch 'wrong-number-of-args (string->number "123" 2 3))

; R7RS Section 6.3 Booleans

#|
not
对单个参数执行逻辑非操作，否定其布尔值。

语法
----
(not obj)

参数
----
obj : any
任意类型的对象。根据R7RS，任何非#f的值都被视为真值。

返回值
------
boolean?
如果传入的对象是#f则返回#t，否则返回#f。

说明
----
1. 逻辑否定：将真值转换为假值，将假值转换为真值。
2. 真值判断：任何非#f的值都被视为真值，包括空列表'()、0、空字符串""等。
3. 布尔运算：门用于布尔值的逻辑取反运算。

示例
----
(not #t) => #f
(not #f) => #t
(not 1) => #f
(not '()) => #f
(not "hello") => #f

错误处理
--------
无错误情况。
|#

;; not 基础测试
(check-false (not #t))          ; #t取反应返回#f
(check-true (not #f))           ; #f取反应返回#t

;; not 真值测试
(check-false (not 1))           ; 任何非#f值都视为真值
(check-false (not 0))           ; 0被视为真值
(check-false (not '()))         ; 空列表被视为真值
(check-false (not 'a))          ; 符号被视为真值
(check-false (not "string"))    ; 字符串被视为真值
(check-false (not #\a))         ; 字符被视为真值

;; not 边界测试
(check-true (not #f))           ; 假值验证
(check-false (not #t))          ; 真值验证

;; not 连续取反测试
(check-true (not (not #t)))     ; 双重否定
(check-false (not (not #f)))    ; 双重否定
(check-true (not (not #t)))     ; 再次验证

;; not 与谓词组合测试
(check-true (not (= 3 4)))      ; 假条件
(check-false (not (= 3 3)))     ; 真条件

;; not 实际用途测试
(check-false (not (list? '(1 2 3))))
(check-true (not (list? 123)))

#|
boolean?
判断一个对象是否为布尔值。

语法
----
(boolean? obj)

参数
----
obj : any
任意类型的对象。

返回值
------
boolean?
如果obj是布尔值（即#t或#f）则返回#t，否则返回#f。

说明
----
1. 用于确定对象是否为布尔值类型
2. 能够正确识别标准的布尔值#t和#f
3. 返回布尔值，便于在类型判断中使用
4. 对所有非布尔对象均返回#f

示例
----
(boolean? #t) => #t
(boolean? #f) => #t
(boolean? 123) => #f
(boolean? "true") => #f
(boolean? 'symbol) => #f

边界情况
--------
- 只识别 #t 和 #f
- 所有非布尔类型的参数都返回 #f
- 可以识别由其他表达式返回的布尔值

错误处理
--------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; boolean? 基础测试
(check (boolean? #t) => #t)
(check (boolean? #f) => #t)

;; boolean? 非布尔类型测试
(check (boolean? 0) => #f)
(check (boolean? 1) => #f)
(check (boolean? -1) => #f)
(check (boolean? 3.14) => #f)
(check (boolean? #\a) => #f)
(check (boolean? "true") => #f)
(check (boolean? "false") => #f)
(check (boolean? "#t") => #f)
(check (boolean? 'true) => #f)
(check (boolean? 'false) => #f)
(check (boolean? 'symbol) => #f)
(check (boolean? '(1 2 3)) => #f)
(check (boolean? '()) => #f)
(check (boolean? #()) => #f)

;; boolean? 复杂类型测试
(check (boolean? (lambda (x) x)) => #f)
(check (boolean? "string") => #f)
(check (boolean? 123.456) => #f)
(check (boolean? #\space) => #f)
(check (boolean? #\newline) => #f)

;; boolean? 布尔返回值测试
(check (boolean? (eq? 1 1)) => #t)
(check (boolean? (= 1 2)) => #t)
(check (boolean? (> 3 2)) => #t)
(check (boolean? (< 1 2)) => #t)
(check (boolean? (zero? 0)) => #t)
(check (boolean? (null? '())) => #t)
(check (boolean? (null? '(1))) => #t)

;; boolean? 特殊边界测试
(check (boolean? #t) => #t)
(check (boolean? #f) => #t)
(check (boolean? 'nil) => #f)
(check (boolean? 't) => #f)
(check (boolean? 'f) => #f)

;; boolean? 与布尔运算结合测试
(check (boolean? (not #t)) => #t)
(check (boolean? (not #f)) => #t)
(check (boolean? (not 123)) => #t)  ;; (not 123) => #t
(check (boolean? (and #t #f)) => #t)
(check (boolean? (or #t #f)) => #t)
(check (boolean? (boolean=? #t #t)) => #t)

;; 类型判断一致性测试
(check (boolean? (boolean? #t)) => #t)
(check (boolean? (boolean? #f)) => #t)
(check (boolean? (boolean? 123)) => #t)
(check (boolean? (string? "hello")) => #t)
(check (boolean? (integer? 42)) => #t)

;; 边界类型测试
(check (boolean? 0/1) => #f)
(check (boolean? 1+2i) => #f)
(check (boolean? +inf.0) => #f)
(check (boolean? -inf.0) => #f)
(check (boolean? +nan.0) => #f)

;; 错误处理测试
(check-catch 'wrong-number-of-args (boolean?))
(check-catch 'wrong-number-of-args (boolean? #t #f))
(check-catch 'wrong-number-of-args (boolean? 1 2 3))


#|
boolean=?
比较两个或多个布尔值是否相等。

语法
----
(boolean=? bool1 bool2 . more-bools)

参数
----
bool1, bool2, ... : boolean?
布尔值，可以是 #t (真) 或 #f (假)。

返回值
------
boolean?
如果所有给定的布尔值都相等，则返回 #t (真)，否则返回 #f (假)。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是布尔值 (#t 或 #f)
3. 当所有布尔值相同时返回 #t，否则返回 #f
4. 支持比较两个或多个布尔值

错误处理
--------
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; boolean=? 基本测试
(check (boolean=? #t #t) => #t)
(check (boolean=? #f #f) => #t)
(check (boolean=? #t #f) => #f)
(check (boolean=? #f #t) => #f)
(check (boolean=? 1 #t) => #f)

;; 多参数测试
(check (boolean=? #t #t #t) => #t)
(check (boolean=? #f #f #f) => #t)
(check (boolean=? #t #t #f) => #f)
(check (boolean=? #t #f #t) => #f)
(check (boolean=? #f #t #t) => #f)

;; 边界测试
(check (boolean=? #t #t #t #t #t) => #t)
(check (boolean=? #f #f #f #f #f) => #t)
(check (boolean=? #t #t #f #t #t) => #f)

;; 错误处理测试
(check-catch 'wrong-number-of-args (boolean=?))
(check-catch 'wrong-number-of-args (boolean=? #t))


(check (apply + (list 3 4)) => 7)
(check (apply + (list 2 3 4)) => 9)

(check (values 4) => 4)
(check (values) => #<unspecified>)

(check (+ (values 1 2 3) 4) => 10)

(check (string-ref ((lambda () (values "abcd" 2)))) => #\c)

(check (+ (call/cc (lambda (ret) (ret 1 2 3))) 4) => 10)

(check (call-with-values (lambda () (values 4 5))
                         (lambda (x y) x))
       => 4
) ;check

(check (*) => 1)
(check (call-with-values * -) => -1)

(check
  (receive (a b) (values 1 2) (+ a b))
  => 3
) ;check

(guard (condition
         (else
          (display "condition: ")
          (write condition)
          (newline)
          'exception)
         ) ;else
  (+ 1 (raise 'an-error))
) ;guard
; PRINTS: condition: an-error

(guard (condition
         (else
          (display "something went wrong")
          (newline)
          'dont-care)
         ) ;else
 (+ 1 (raise 'an-error))
) ;guard
; PRINTS: something went wrong

(with-input-from-string "(+ 1 2)"
  (lambda ()
    (let ((datum (read))) 
      (check-true (list? datum))
      (check datum => '(+ 1 2))
    ) ;let
  ) ;lambda
) ;with-input-from-string

(check (eof-object) => #<eof>)

(check-true ((compose not zero?) 1))
(check-false ((compose not zero?) 0))

(check (let1 x 1 x) => 1)
(check (let1 x 1 (+ x 1)) => 2)

(let1 add1/add (lambda* (x (y 1)) (+ x y))
  (check (add1/add 1) => 2)
  (check (add1/add 0) => 1)
  (check (add1/add 1 2)=> 3)
) ;let1

(define add3
  (typed-lambda
    ((i integer?) (x real?) z)
    (+ i x z)
  ) ;typed-lambda
) ;define

(check (add3 1 2 3) => 6)
(check-catch 'type-error (add3 1.2 2 3))


#|
symbol?
判断给定的对象是否为符号(symbol)类型

语法
----
(symbol? obj)

参数
----
obj : any
任意类型的对象

返回值
-----
boolean?
如果obj是符号类型则返回#t，否则返回#f

说明
----
符号是Scheme中的基本数据类型之一，用单引号(')前缀表示。
符号在Scheme中通常用作标识符、关键字或枚举值。

|#

(check-true (symbol? 'foo))
(check-true (symbol? (car '(foo bar))))
(check-true (symbol? 'nil))

(check-false (symbol? "bar"))
(check-false (symbol? #f))
(check-false (symbol? '()))
(check-false (symbol? '123))

;; 边界情况测试
(check-true (symbol? '+))
(check-true (symbol? '-))
(check-true (symbol? '*))
(check-true (symbol? '/))
(check-true (symbol? '==))
(check-true (symbol? '=>))

;; 数字开头符号测试
(check-true (symbol? '123abc))
(check-true (symbol? '1a2b3c))

;; 空符号名称测试 (注意：某些scheme系统可能不支持空符号)
(check-true (symbol? (string->symbol "empty-symbol")))

;; 特殊符号测试
(check-true (symbol? 'if))
(check-true (symbol? 'lambda))
(check-true (symbol? 'define))
(check-true (symbol? 'let))
(check-true (symbol? 'begin))

;; 特殊符号格式测试
(check-true (symbol? 'complex_name))
(check-true (symbol? 'symbol_with_underscore))
(check-true (symbol? 'symbol-with-dash))

;; 非符号类型测试
(check-false (symbol? 123))
(check-false (symbol? 123.456))
(check-false (symbol? #\a))
(check-false (symbol? '()))
(check-false (symbol? (list 'a 'b 'c)))
(check-false (symbol? (vector 'a 'b 'c)))

;; 字符串转换测试
(check-true (symbol? (string->symbol "test")))
(check-true (symbol? (string->symbol "complex-symbol-with-numbers")))

#|
symbol=?
判断给定符号是否相等

语法
----
(symbol=? symbol1 symbol2 ...)

参数
----
symbol1, symbol2, ... : symbol?
一个或多个符号参数

返回值
-----
boolean?
如果所有符号相等则返回#t，否则返回#f

说明
----
符号比较是基于符号的标识符名称进行的。
R7RS中规定symbol=?需要至少两个参数。

|#

;; 基本测试
(check-catch 'wrong-number-of-args (symbol=? 'a))
(check-catch 'wrong-number-of-args (symbol=? 1))

(check-true (symbol=? 'a 'a))
(check-true (symbol=? 'foo 'foo))
(check-false (symbol=? 'a 'b))
(check-false (symbol=? 'foo 'bar))

;; 多参数测试
(check-true (symbol=? 'bar 'bar 'bar))
(check-true (symbol=? 'x 'x 'x 'x))
(check-false (symbol=? 'a 'a 'b))

;; 边界测试
(check-true (symbol=? 'a (string->symbol "a")))
(check-false (symbol=? 'a (string->symbol "A")))

;; 类型错误测试
(check-false (symbol=? 1 1))
(check-false (symbol=? 'a 1))
(check-false (symbol=? (string->symbol "foo") 1))
(check-false (symbol=? 'a 'b '()))

#|
symbol->string
将符号转换为字符串形式

语法
----
(symbol->string symbol)

参数
----
symbol : symbol?
要转换的符号

返回值
-----
string?
符号对应的字符串表示

错误
----
wrong-type-arg
如果参数不是符号类型，抛出错误。

说明
----
symbol->string将符号的标识符转换为等效的字符串表示。
注意区分大小写：符号'abc和'ABC会转换为"abc"和"ABC"的不同字符串。

|#

;; 基本测试
(check (symbol->string 'MathAgape) => "MathAgape")
(check (symbol->string 'goldfish-scheme) => "goldfish-scheme")
(check (symbol->string (string->symbol "Hello World")) => "Hello World")

;; 特殊符号测试
(check (symbol->string '+) => "+")
(check (symbol->string '-) => "-")
(check (symbol->string '*) => "*")
(check (symbol->string '/) => "/")
(check (symbol->string '=>) => "=>")
(check (symbol->string '<=) => "<=")

;; 大小写敏感测试
(check (symbol->string 'ABC) => "ABC")
(check (symbol->string 'abc) => "abc")
(check (symbol->string 'LispCase) => "LispCase")
(check (symbol->string 'camelCase) => "camelCase")

;; 边界测试
(check (symbol->string 'a) => "a")
(check (symbol->string 'x) => "x")
(check (symbol->string 'empty) => "empty")

;; 数字和特殊字符测试
(check (symbol->string (string->symbol "123")) => "123")
(check (symbol->string (string->symbol "123abc")) => "123abc")
(check (symbol->string (string->symbol "symbol_with_underscore")) => "symbol_with_underscore")
(check (symbol->string (string->symbol "symbol-with-dash")) => "symbol-with-dash")
(check (symbol->string (string->symbol "sym$bol")) => "sym$bol")

;; 错误测试
(check-catch 'wrong-type-arg (symbol->string 123))
(check-catch 'wrong-type-arg (symbol->string "symbol"))
(check-catch 'wrong-type-arg (symbol->string #f))
(check-catch 'wrong-type-arg (symbol->string '()))
(check-catch 'wrong-number-of-args (symbol->string 'a 'b))
(check-catch 'wrong-number-of-args (symbol->string))

;; 往返转换测试
(let ((test-symbols '(hello world scheme-prog example complex-identifier my-symbol)))
  (for-each
    (lambda (sym)
      (let ((str (symbol->string sym)))
        (check (string->symbol str) => sym)
      ) ;let
    ) ;lambda
    test-symbols
  ) ;for-each
) ;let

#|
string->symbol
将字符串转换为对应的符号

语法
----
(string->symbol string)

参数
----
string : string?
要转换的字符串。可以是空字符串、包含数字、特殊字符的任何字符串内容。

返回值
-----
symbol?
字符串对应的符号标识符。

错误
----
wrong-type-arg
如果参数不是字符串类型，抛出错误。
wrong-number-of-args
如果没有参数或参数数量超过一个，抛出错误。

行为特性
--------
1. 名称转换：字符串内容会原样转换为符号标识符，保持大小写敏感
2. 数字处理：数字字符串（如"123"）转换为数字名称符号，而非数值123
3. 重入一致性：相同字符串多次转换返回同一个符号对象

|#

;; 基本转换测试
(check (string->symbol "MathAgape") => `MathAgape)
(check (string->symbol "hello") => 'hello)
(check (string->symbol "scheme-prog") => 'scheme-prog)

;; 特殊字符转换测试
(check (string->symbol "+") => '+)
(check (string->symbol "-") => '-)
(check (string->symbol "*") => '*)
(check (string->symbol "lambda") => 'lambda)

;; 大小写敏感测试
(check (string->symbol "ABC") => 'ABC)
(check (string->symbol "abc") => 'abc)

;; 数字符号化处理（重要区别）
(check-false (equal? (string->symbol "123") 123))   ; 不是数值

;; 混合字符测试
(check (string->symbol "123abc") => (string->symbol "123abc"))
(check (string->symbol "symbol-with-dash") => (string->symbol "symbol-with-dash"))
(check (string->symbol "symbol_with_underscore") => (string->symbol "symbol_with_underscore"))

;; 错误处理测试  
(check-catch 'wrong-type-arg (string->symbol 123))
(check-catch 'wrong-type-arg (string->symbol 'symbol))
(check-catch 'wrong-number-of-args (string->symbol "a" "b"))
(check-catch 'wrong-number-of-args (string->symbol))

;; 保留字符号化
(check (string->symbol "if") => 'if)
(check (string->symbol "define") => 'define)
(check (string->symbol "let") => 'let)

;; 保持原始往返测试
(check (string->symbol (symbol->string `MathAgape)) => `MathAgape)


#|
char?
判断对象是否为字符的谓词。

语法
----
(char? obj)

参数
----
obj : any?
任意对象。

返回值
------
boolean?
如果对象是字符则返回 #t，否则返回 #f。

说明
----
1. 用于检查对象是否为字符类型
2. 能够正确识别各种字符形式：字母、数字、特殊字符等
3. 返回布尔值，便于在条件判断中使用

错误处理
--------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; char? 基础测试
(check (char? #\A) => #t)
(check (char? #\a) => #t)
(check (char? #\0) => #t)
(check (char? #\space) => #t)
(check (char? #\!) => #t)
(check (char? 123) => #f)
(check (char? "A") => #f)
(check (char? 'a) => #f)

;; 错误处理测试
(check-catch 'wrong-number-of-args (char?))
(check-catch 'wrong-number-of-args (char? #\A #\B))

#|
char=?
比较两个或多个字符是否相等。

语法
----
(char=? char1 char2 . more-chars)

参数
----
char1, char2, ... : char?
字符值。

返回值
------
boolean?
如果所有给定的字符都相等，则返回 #t (真)，否则返回 #f (假)。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是字符
3. 当所有字符相等时返回 #t，否则返回 #f
4. 支持比较两个或多个字符
5. 区分大小写

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; char=? 基本测试
(check (char=? #\A #\A) => #t)
(check (char=? #\a #\a) => #t)
(check (char=? #\A #\a) => #f)
(check (char=? #\a #\A) => #f)
(check (char=? #\0 #\0) => #t)
(check (char=? #\9 #\9) => #t)
(check (char=? #\0 #\9) => #f)

;; 特殊字符测试
(check (char=? #\space #\space) => #t)
(check (char=? #\newline #\newline) => #t)
(check (char=? #\tab #\tab) => #t)
(check (char=? #\space #\newline) => #f)

;; 多参数测试
(check (char=? #\A #\A #\A) => #t)
(check (char=? #\a #\a #\a) => #t)
(check (char=? #\A #\A #\a) => #f)
(check (char=? #\a #\b #\c) => #f)

;; 边界测试
(check (char=? #\0 #\0 #\0 #\0 #\0) => #t)
(check (char=? #\A #\A #\A #\A #\a) => #f)
(check (char=? #\z #\z #\z) => #t)
(check (char=? #\! #\! #\!) => #t)

;; 数字字符测试
(check (char=? #\1 #\1) => #t)
(check (char=? #\1 #\! ) => #f)

;; 大小写混合测试
(check (char=? #\a #\b #\c #\d) => #f)
(check (char=? #\A #\B #\C) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char=? 1 #\A))
(check-catch 'wrong-type-arg (char=? #\A 'symbol))
(check-catch 'wrong-type-arg (char=? 123 #\a))
(check-catch 'wrong-number-of-args (char=?))
(check-catch 'wrong-number-of-args (char=? #\A))


#|
char<?
按字典序比较字符的大小，判断字符是否按升序排列。

语法
----
(char<? char1 char2 char3 ...)

参数
----
char1, char2, char3, ... : char?
要比较的字符，至少需要两个。

返回值
------
boolean?
如果所有字符按升序排列（即每个字符都小于下一个字符）则返回 #t，否则返回 #f。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是字符
3. 按字符的Unicode码点值进行比较
4. 当字符按严格升序排列时返回 #t，否则返回 #f
5. 区分大小写，大写字符码点值小于小写字符（如 #\A < #\a）

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; char<? 基本测试
(check (char<? #\A #\B) => #t)
(check (char<? #\a #\b) => #t)
(check (char<? #\A #\a) => #t)  ; 大写小于小写
(check (char<? #\a #\A) => #f)  ; 小写不小于大写
(check (char<? #\0 #\9) => #t)
(check (char<? #\9 #\0) => #f)

;; 相等字符测试
(check (char<? #\A #\A) => #f)
(check (char<? #\a #\a) => #f)
(check (char<? #\0 #\0) => #f)

;; 特殊字符测试
(check (char<? #\space #\newline) => #f)
(check (char<? #\tab #\space) => #t)
(check (char<? #\newline #\tab) => #f)

;; 多参数升序测试
(check (char<? #\A #\B #\C) => #t)
(check (char<? #\a #\b #\c) => #t)
(check (char<? #\0 #\1 #\2 #\3 #\4) => #t)
(check (char<? #\! #\# #\$ #\%) => #t)

;; 多参数非升序测试
(check (char<? #\A #\B #\A) => #f)
(check (char<? #\a #\a #\b) => #f)  ; 等于不满足小于关系
(check (char<? #\3 #\2 #\1) => #f)

;; 混合大小写测试
(check (char<? #\A #\a #\b) => #t)
(check (char<? #\Z #\a #\z) => #t)
(check (char<? #\a #\Z #\b) => #f)

;; 边界测试
(check (char<? #\0 #\9) => #t)
(check (char<? #\A #\Z) => #t)
(check (char<? #\a #\z) => #t)
(check (char<? #\! #\~) => #t)

;; 数字字符测试
(check (char<? #\1 #\2) => #t)
(check (char<? #\5 #\5) => #f)
(check (char<? #\9 #\8) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char<? 1 #\A))
(check-catch 'wrong-type-arg (char<? #\A 'symbol))
(check-catch 'wrong-type-arg (char<? 123 #\a))
(check-catch 'wrong-number-of-args (char<?))
(check-catch 'wrong-number-of-args (char<? #\A))


#|
char>?
按字典序比较字符的大小，判断字符是否按降序排列。

语法
----
(char>? char1 char2 char3 ...)

参数
----
char1, char2, char3, ... : char?
要比较的字符，至少需要两个。

返回值
------
boolean?
如果所有字符按降序排列（即每个字符都大于下一个字符）则返回 #t，否则返回 #f。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是字符
3. 按字符的Unicode码点值进行比较
4. 当字符按严格降序排列时返回 #t，否则返回 #f
5. 区分大小写，大写字符码点值小于小写字符（如 #\A < #\a）

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; char>? 基本测试
(check (char>? #\B #\A) => #t)
(check (char>? #\b #\a) => #t)
(check (char>? #\a #\A) => #t)  ; 小写大于大写
(check (char>? #\A #\a) => #f)  ; 大写不大于小写
(check (char>? #\9 #\0) => #t)
(check (char>? #\0 #\9) => #f)

;; 相等字符测试
(check (char>? #\A #\A) => #f)
(check (char>? #\a #\a) => #f)
(check (char>? #\0 #\0) => #f)

;; 特殊字符测试
(check (char>? #\newline #\space) => #f)
(check (char>? #\space #\tab) => #t)
(check (char>? #\tab #\newline) => #f)

;; 多参数降序测试
(check (char>? #\C #\B #\A) => #t)
(check (char>? #\c #\b #\a) => #t)
(check (char>? #\4 #\3 #\2 #\1 #\0) => #t)
(check (char>? #\% #\$ #\# #\! #\~) => #f)

;; 多参数非降序测试
(check (char>? #\B #\A #\B) => #f)
(check (char>? #\a #\a #\b) => #f)  ; 等号不满足大于关系
(check (char>? #\1 #\2 #\3) => #f)

;; 混合大小写测试
(check (char>? #\b #\a #\Z) => #t)
(check (char>? #\z #\a #\Z) => #t)
(check (char>? #\A #\Z #\a) => #f)

;; 边界测试
(check (char>? #\9 #\0) => #t)
(check (char>? #\Z #\A) => #t)
(check (char>? #\z #\a) => #t)
(check (char>? #\~ #\! ) => #t)

;; 数字字符测试
(check (char>? #\2 #\1) => #t)
(check (char>? #\5 #\5) => #f)
(check (char>? #\8 #\9) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char>? 1 #\A))
(check-catch 'wrong-type-arg (char>? #\A 'symbol))
(check-catch 'wrong-number-of-args (char>?))
(check-catch 'wrong-number-of-args (char>? #\A))


#|
char<=?
按字典序比较字符的大小，判断字符是否按非严格升序排列。

语法
----
(char<=? char1 char2 char3 ...)

参数
----
char1, char2, char3, ... : char?
要比较的字符，至少需要两个。

返回值
------
boolean?
如果所有字符按非严格升序排列（即每个字符都小于或等于下一个字符）则返回 #t，否则返回 #f。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是字符
3. 按字符的Unicode码点值进行比较
4. 当字符按非严格升序排列时返回 #t，否则返回 #f
5. 区分大小写，大写字符码点值小于小写字符
6. 允许字符相等的情况

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; char<=? 基本测试
(check (char<=? #\A #\B) => #t)
(check (char<=? #\a #\b) => #t)
(check (char<=? #\A #\A) => #t)  ; 相等情况返回 #t
(check (char<=? #\B #\A) => #f)  ; 大于返回 #f
(check (char<=? #\0 #\9) => #t)
(check (char<=? #\9 #\0) => #f)

;; 相等字符测试
(check (char<=? #\A #\A) => #t)
(check (char<=? #\a #\a #\a) => #t)  ; 全部为相同时返回 #t
(check (char<=? #\0 #\0) => #t)

;; 特殊字符测试
(check (char<=? #\space #\newline) => #f)
(check (char<=? #\tab #\tab) => #t)  ; 相等返回 true
(check (char<=? #\newline #\space) => #t)

;; 多参数非严格升序测试
(check (char<=? #\A #\B #\C) => #t)
(check (char<=? #\A #\A #\B) => #t)  ; 允许相等
(check (char<=? #\a #\b #\c) => #t)
(check (char<=? #\0 #\0 #\1 #\1 #\2) => #t)

;; 多参数非升序测试
(check (char<=? #\A #\B #\A) => #f)
(check (char<=? #\b #\a) => #f)
(check (char<=? #\3 #\2 #\1) => #f)

;; 混合大小写测试
(check (char<=? #\A #\A #\a) => #t)
(check (char<=? #\Z #\a) => #t)
(check (char<=? #\b #\a #\Z) => #f)

;; 边界测试
(check (char<=? #\0 #\1 #\9) => #t)
(check (char<=? #\A #\Z) => #t)
(check (char<=? #\! #\~ #\~) => #t)

;; 数字字符测试
(check (char<=? #\1 #\2) => #t)
(check (char<=? #\5 #\5) => #t)
(check (char<=? #\9 #\8) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char<=? 1 #\A))
(check-catch 'wrong-type-arg (char<=? #\A 'symbol))
(check-catch 'wrong-number-of-args (char<=?))
(check-catch 'wrong-number-of-args (char<=? #\A))


#|
char>=?
按字典序比较字符的大小，判断字符是否按非严格降序排列。

语法
----
(char>=? char1 char2 char3 ...)

参数
----
char1, char2, char3, ... : char?
要比较的字符，至少需要两个。

返回值
------
boolean?
如果所有字符按非严格降序排列（即每个字符都大于或等于下一个字符）则返回 #t，否则返回 #f。

说明
----
1. 至少需要两个参数
2. 所有参数必须都是字符
3. 按字符的Unicode码点值进行比较
4. 当字符按非严格降序排列时返回 #t，否则返回 #f
5. 区分大小写，大写字符码点值小于小写字符
6. 允许字符相等的情况

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; char>=? 基本测试
(check (char>=? #\B #\A) => #t)
(check (char>=? #\b #\a) => #t)
(check (char>=? #\A #\A) => #t)  ; 相等情况返回 #t
(check (char>=? #\A #\B) => #f)  ; 小于返回 #f
(check (char>=? #\9 #\0) => #t)
(check (char>=? #\0 #\9) => #f)

;; 相等字符测试
(check (char>=? #\A #\A) => #t)
(check (char>=? #\a #\a #\a) => #t)  ; 全部为相同时返回 #t
(check (char>=? #\0 #\0) => #t)

;; 特殊字符测试
(check (char>=? #\newline #\space) => #f)
(check (char>=? #\tab #\tab) => #t)  ; 相等返回 true
(check (char>=? #\space #\newline) => #t)

;; 多参数非严格降序测试
(check (char>=? #\C #\B #\A) => #t)
(check (char>=? #\B #\B #\A) => #t)  ; 允许相等
(check (char>=? #\c #\b #\a) => #t)
(check (char>=? #\2 #\2 #\1 #\1 #\0) => #t)

;; 多参数非降序测试
(check (char>=? #\B #\A #\B) => #f)
(check (char>=? #\a #\b) => #f)
(check (char>=? #\1 #\2 #\3) => #f)

;; 混合大小写测试
(check (char>=? #\a #\a #\Z) => #t)
(check (char>=? #\z #\a) => #t)
(check (char>=? #\Z #\a #\b) => #f)

;; 边界测试
(check (char>=? #\9 #\8 #\0) => #t)
(check (char>=? #\Z #\A) => #t)
(check (char>=? #\~ #\~ #\! ) => #t)

;; 数字字符测试
(check (char>=? #\2 #\1) => #t)
(check (char>=? #\5 #\5) => #t)
(check (char>=? #\8 #\9) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char>=? 1 #\A))
(check-catch 'wrong-type-arg (char>=? #\A 'symbol))
(check-catch 'wrong-number-of-args (char>=?))
(check-catch 'wrong-number-of-args (char>=? #\A))













#|
char->integer
将字符转换为其对应的码点值。

语法
----
(char->integer char)

参数
----
char : char?
字符。

返回值
------
integer?
字符对应的码点值

说明
----
将字符转换为对应的整数值

错误处理
--------
wrong-type-arg
当参数不是字符时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; char->integer 基本测试
(check (char->integer #\0) => 48)
(check (char->integer #\9) => 57)


;; 字符边界测试
(check (char->integer #\tab) => 9)
(check (char->integer #\newline) => 10)
(check (char->integer #\return) => 13)
(check (char->integer #\backspace) => 8)

;; 特殊字符测试
(check (char->integer #\!) => 33)
(check (char->integer #\@) => 64)
(check (char->integer #\#) => 35)
(check (char->integer #\$) => 36)
(check (char->integer #\%) => 37)

;; 扩展字符测试
(check (char->integer #\~) => 126)
(check (char->integer #\_) => 95)

;; 数字边界测试
(check (char->integer #\A) => 65)
(check (char->integer #\B) => 66)
(check (char->integer #\Z) => 90)
(check (char->integer #\a) => 97)
(check (char->integer #\z) => 122)

;; 错误处理测试
(check-catch 'wrong-type-arg (char->integer 65))
(check-catch 'wrong-type-arg (char->integer "A"))
(check-catch 'wrong-number-of-args (char->integer))
(check-catch 'wrong-number-of-args (char->integer #\A #\B))

#|
integer->char
将整数码点转换为对应的字符。

语法
----
(integer->char n)

参数
----
n : integer?
整数值，必须是有效的码点值，通常范围在0到255之间。
返回值
------
char?
对应的字符

说明
----
1. 将整数转换为对应的字符
4. 与char->integer互逆操作


错误处理
--------
out-of-range
当码点超出有效范围时抛出错误。
wrong-type-arg
当参数不是整数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; integer->char 基本测试
(check (integer->char 65) => #\A)
(check (integer->char 97) => #\a)
(check (integer->char 48) => #\0)
(check (integer->char 57) => #\9)
(check (integer->char 10) => #\newline)
(check (integer->char 32) => #\space)
(check (integer->char 9) => #\tab)

;; 大写和小写字符
(check (integer->char 65) => #\A)
(check (integer->char 90) => #\Z)
(check (integer->char 97) => #\a)
(check (integer->char 122) => #\z)

;; 数字字符
(check (integer->char 48) => #\0)
(check (integer->char 49) => #\1)
(check (integer->char 57) => #\9)

;; 特殊字符测试
(check (integer->char 33) => #\!)
(check (integer->char 64) => #\@)
(check (integer->char 35) => #\#)

;; 边界测试
(check (integer->char 0) => #\null)
(check (integer->char 126) => #\~)

;; 反向验证
(check (integer->char (char->integer #\A)) => #\A)
(check (integer->char (char->integer #\a)) => #\a)
(check (integer->char (char->integer #\0)) => #\0)
(check (char->integer (integer->char 65)) => 65)
(check (char->integer (integer->char 97)) => 97)

;; 错误处理测试
(check-catch 'out-of-range (integer->char -1))
(check-catch 'out-of-range (integer->char 256))
(check-catch 'wrong-type-arg (integer->char 65.0))
(check-catch 'wrong-number-of-args (integer->char))
(check-catch 'wrong-number-of-args (integer->char 65 66))  

#|
string?
判断给定的对象是否为字符串类型。

语法
----
(string? obj)

参数
----
obj : any
任意类型的对象

返回值
------
boolean?
如果obj是字符串类型则返回#t，否则返回#f

说明
----
1. 用于检查对象是否为字符串类型
2. 能够正确识别空字符串和非空字符串
3. 返回布尔值，便于在条件判断中使用

错误处理
--------
wrong-number-of-args
当参数数量不为1时抛出错误。

|#

;; string? 基本测试
(check (string? "hello") => #t)
(check (string? "") => #t)
(check (string? "世界") => #t)
(check (string? "123") => #t)
(check-true (string? "MathAgape"))

;; 非字符串类型测试
(check-false (string? 'a-symbol))
(check-false (string? 123))
(check-false (string? #t))
(check-false (string? #f))
(check-false (string? '()))
(check-false (string? '(1 2 3)))
(check-false (string? #(1 2 3)))
(check-false (string? 3.14))

;; 边界情况测试
(check (string? "\n") => #t)
(check (string? "\t") => #t)
(check (string? " ") => #t)

;; 特殊字符测试
(check (string? "$$$") => #t)
(check (string? "中国") => #t)


#|
make-string
创建一个由指定字符重复填充的新字符串。

语法
----
(make-string k [char])

参数
----
k : exact?
必须是非负的精确整数，表示要创建的字符串长度。

char : char? 可选
用于填充字符串的字符。如果未提供，**默认字符由实现定义**。

返回值
------
string?
新创建的字符串，长度为 k，所有字符均为 char（或实现定义的默认字符）。

说明
----
1. 可以指定字符串长度和填充字符
2. 若未指定 char，**默认字符未在 R7RS 中定义**
3. 当 k 为 0 时返回空字符串 ""

错误处理
--------
out-of-range
当 k 为负数时抛出错误。
wrong-type-arg
当 k 不是精确整数或 char 不是字符时抛出错误。
wrong-number-of-args
当参数数量不为 1 或 2 个时抛出错误。

|#

(check (string-length (make-string 0)) => 0)
(check (string-length (make-string 1)) => 1)
(check (string-length (make-string 1000)) => 1000)
(check (string-length (make-string 1000000)) => 1000000)

(check (make-string 0 #\a) => "")
(check (make-string 1 #\a) => "a")

(check (string-length (make-string 1000 #\a)) => 1000)
(let1 str (make-string 10000 #\a)
  (check (string-length str) => 10000)
  (check (string-ref str 0) => #\a)
  (check (string-ref str 9999) => #\a)
) ;let1

(check-catch 'out-of-range (make-string -1))
(check-catch 'out-of-range (make-string -5 #\a))
(check-catch 'wrong-type-arg (make-string 3.5))
(check-catch 'wrong-type-arg (make-string 3 "a"))
(check-catch 'wrong-number-of-args (make-string))
(check-catch 'wrong-number-of-args (make-string 3 #\a #\b))


(check (string->list "MathAgape")
  => '(#\M #\a #\t #\h #\A #\g #\a #\p #\e)
) ;check

(check (string->list "") => '())

(check
  (list->string '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))
  => "MathAgape"
) ;check

(check (list->string '()) => "")

(check (string-length "MathAgape") => 9)
(check (string-length "") => 0)

(check
  (catch 'wrong-type-arg
    (lambda () (string-length 'not-a-string))
    (lambda args #t)
  ) ;catch
  =>
  #t
) ;check

#|
string-length
返回给定字符串在UTF-8编码下的字节长度。

语法
----
(string-length string)

参数
----
string : string?
要测量长度的字符串，可以是空字符串、单字符字符串或多字符字符串。

返回值
------
integer?
返回一个非负整数，表示字符串在UTF-8编码下的字节长度。

说明
----
1. 字符串长度计算包含所有字节编码单元，包括空格、制表符和换行符
2. 空字符串 "" 的长度为 0
3. 对于ASCII字符（0-127），每个字符占用1字节
4. 对于非ASCII字符，每个字符可能占用2-4字节UTF-8编码单元
5. 字符串不会改变原始数据，只是返回长度信息

边界情况
--------
- 空字符串长度：0
- ASCII字符长度：1字节/字符
- UTF-8非ASCII字符：2-4字节/字符
- 多字节编码字符串：总字节长度

错误处理
--------
wrong-type-arg
当参数不是字符串类型时抛出错误。
wrong-number-of-args
当参数数量不为1个时抛出错误。

|#

;; string-length 基础测试
(check (string-length "") => 0)
(check (string-length "a") => 1)
(check (string-length "hello") => 5)
(check (string-length "世界") => 6)
(check (string-length "你好世界") => 12)

;; 空字符串测试
(check (string-length "") => 0)
(check (string-length (list->string '())) => 0)

;; 单字符测试
(check (string-length "a") => 1)
(check (string-length "A") => 1)
(check (string-length "1") => 1)
(check (string-length "!") => 1)
(check (string-length " ") => 1)

;; 多字符测试
(check (string-length "abc") => 3)
(check (string-length "ABC") => 3)
(check (string-length "123") => 3)
(check (string-length "!@#") => 3)

;; 含有空格的字符串
(check (string-length "hello world") => 11)
(check (string-length "  ") => 2)
(check (string-length " leading space") => 14)
(check (string-length "trailing space ") => 15)

;; 特殊字符和空白字符
(check (string-length "hello\nworld") => 11)
(check (string-length "tab\tseparated") => 13)
(check (string-length "line\rreturn") => 11)

;; Unicode字符测试 - 按字节编码单元计数
(check (string-length "😀") => 4)  ; emoji 4字节UTF-8
(check (string-length "μ") => 2)   ; 希腊字母 2字节UTF-8
(check (string-length "ä") => 2)   ; 变音符号 2字节UTF-8
(check (string-length "中文") => 6) ; 中文字符 每个3字节UTF-8

;; 长度边界测试
(check (string-length "a") => 1)
(check (string-length "abcdefghijklmnop") => 16)
(check (string-length "abcdefghijklmnopqrstuvwxyz") => 26)
(check (string-length "aaaaaaaaaaaaaaaaaaaaaaaaaa") => 26)

;; 与字符串生成函数的兼容性测试
(check (string-length (make-string 5 #\a)) => 5)
(check (string-length (make-string 10 #\x)) => 10)
(check (string-length (make-string 0)) => 0)

;; 与字符串拼接函数的兼容性测试
(check (string-length (string-append "hello" "world")) => 10)
(check (string-length (string-append "" "")) => 0)
(check (string-length (string-append "a" "b")) => 2)

;; 错误处理测试
(check-catch 'wrong-type-arg (string-length 123))
(check-catch 'wrong-type-arg (string-length 'symbol))
(check-catch 'wrong-type-arg (string-length #t))
(check-catch 'wrong-type-arg (string-length '()))
(check-catch 'wrong-type-arg (string-length #(1 2 3)))
(check-catch 'wrong-number-of-args (string-length))
(check-catch 'wrong-number-of-args (string-length "hello" "world"))
(check-catch 'wrong-number-of-args (string-length "hello" 1))


#|
string-ref
按索引访问字符串中的字符。

语法
----
(string-ref string k)

参数
----
string : string?
要访问的字符串

k : exact?
必须是精确的整数索引，从0开始。必须满足 0 <= k < (string-length string)

返回值
------
char?
字符串中位置k处的字符

说明
----
1. 从0开始索引
2. 在R7RS中，索引k必须在有效范围内
3. 返回k位置处的字符

错误处理
--------
out-of-range
当k为负数或大于等于字符串长度时抛出错误。

wrong-type-arg
当string不是字符串或k不是精确整数时抛出错误。

错误
----
当索引超出范围时，会抛出out-of-range异常。
|#

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

(let1 v #(1 2 3)
  (check (vector-ref v 0) => 1)
  (check (v 0) => 1)
  
  (check (vector-ref v 2) => 3)
  (check (v 2) => 3)
) ;let1

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


#|
string-set!
修改字符串中指定位置的字符，返回修改后的字符串。在R7RS标准中，string-set!是一个立即执行的变异操作，不会创建新的字符串对象。

语法
----
(string-set! string k char)

参数
----
string : string? 
要修改的原始字符串。必须是非常量字符串。

k : exact?
必须是非负的精确整数，表示要修改的字符索引位置。必须小于字符串长度。

char : char?
新的字符值，用于替换位置k处的原始字符。

返回值
------
unspecified
按照R7RS规范，返回未指定的值。

说明
----
1. 这是一个变异操作，会直接修改原始字符串对象的内容
2. 索引k是从0开始计算的
3. 可以用来修改任何位置的合法字符，但不能用于扩展字符串长度
4. 修改后的字符串与新字符串不同的引用指向相同的内存内容
5. 参数必须是变量引用或动态创建的字符串，不能是字符串常量

错误处理
--------
out-of-range
当索引k为负数或大于等于字符串长度时抛出错误。

wrong-type-arg
当string不是字符串、k不是精确整数、char不是字符时抛出错误。
|#

;; string-set! 基础测试
(let1 str (string-copy "hello")
  (string-set! str 1 #\A)
  (check str => "hAllo")
) ;let1

(let1 str (string-copy "abc")
  (string-set! str 0 #\X)
  (string-set! str 2 #\Z)
  (check str => "XbZ")
) ;let1

;; 修改不同位置测试
(let1 str (string-copy "123456")
  (string-set! str 0 #\0)
  (string-set! str 5 #\9)
  (check str => "023459")
) ;let1

;; 边界位置测试
(let1 str (string-copy "a") 
  (string-set! str 0 #\A)
  (check str => "A")
) ;let1

(let1 str (string-copy "xyz")
  (string-set! str 0 #\1)
  (string-set! str 1 #\2) 
  (string-set! str 2 #\3)
  (check str => "123")
) ;let1

;; 特殊字符测试
(let1 str (string-copy "hello world")
  (string-set! str 5 #\-)
  (check str => "hello-world")
) ;let1

(let1 str (string-copy "Test!")
  (string-set! str 4 #\?)
  (check str => "Test?")
) ;let1

;; 数字字符串测试
(let1 str (string-copy "00000")
  (string-set! str 2 #\1)
  (check str => "00100")
) ;let1

;; 连续多次修改
(let1 str (string-copy "original")
  (string-set! str 0 #\O)
  (string-set! str 1 #\R)
  (string-set! str 2 #\I)
  (string-set! str 3 #\G)
  (string-set! str 4 #\I)
  (string-set! str 5 #\N)
  (string-set! str 6 #\A)
  (string-set! str 7 #\L)
  (check str => "ORIGINAL")
) ;let1

;; 测试索引在有效范围内
(let1 str (string-copy "test")
  (string-set! str 0 #\T)
  (string-set! str 1 #\E)
  (string-set! str 2 #\S)
  (string-set! str 3 #\T)
  (check str => "TEST")
) ;let1

;; 错误处理测试
;; 索引越界测试
(let1 str (string-copy "abc")
  (check-catch 'out-of-range (string-set! str -1 #\x))
  (check-catch 'out-of-range (string-set! str 3 #\x))
) ;let1

(let1 str (string-copy "")
  (check-catch 'out-of-range (string-set! str 0 #\x))
) ;let1

;; 类型错误测试
(check-catch 'wrong-type-arg (string-set! 123 0 #\A))
(check-catch 'wrong-type-arg (string-set! "hello" 0.5 #\A))
(check-catch 'wrong-type-arg (string-set! "hello" 0 123))
(check-catch 'wrong-type-arg (string-set! "hello" 1 "A"))

;; 参数数量错误测试
(check-catch 'wrong-number-of-args (string-set!))
(check-catch 'wrong-number-of-args (string-set! "hello"))
(check-catch 'wrong-number-of-args (string-set! "hello" 1))
(check-catch 'wrong-number-of-args (string-set! "hello" 1 #\a #\b))

;; 变量引用一致性测试
(let1 str1 (string-copy "hello")
  (let1 str2 str1
    (string-set! str1 1 #\E)
    (check str1 => "hEllo")
    (check str2 => "hEllo")
  ) ;let1
) ;let1

;; 与string-ref结合使用测试
(let1 str (string-copy "test")
  (check (string-ref str 0) => #\t)
  (string-set! str 0 #\T)
  (check (string-ref str 0) => #\T)
  (check str => "Test")
) ;let1

;; 复杂字符串修改场景测试
(let1 str (string-copy "programming")
  (string-set! str 0 #\P)
  (string-set! str 8 #\N)
  (string-set! str 10 #\G)
  (check (string-ref str 0) => #\P)
  (check (string-ref str 8) => #\N)
  (check (string-ref str 10) => #\G)
) ;let1

#|
string=?
比较两个字符串是否相等。

语法
----
(string=? string1 string2 ...)

参数
----
string1, string2, ... : string?
至少两个字符串参数。

返回值
------
boolean?
如果所有字符串都相等则返回 #t，否则返回 #f。比较区分大小写。

说明
----
1. 至少需要两个参数
2. 所有参数必须是字符串类型
3. 字符串比较区分大小写
4. 当所有字符串内容完全相同返回 #t
5. 返回布尔值结果

边界情况
--------
- 区分大小写：大写和小写字母被认为不同
- 空字符串比较：空字符串只与空字符串相等
- 特殊字符：所有字符都需要完全相同，包括空格、制表符等

错误处理
--------
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; string=? 基本测试
(check-true (string=? "hello" "hello"))
(check-true (string=? "" ""))
(check-true (string=? "a" "a"))

;; string=? 区分大小写测试
(check-false (string=? "Hello" "hello"))
(check-false (string=? "HELLO" "hello"))
(check-false (string=? "abc" "ABC"))

;; string=? 不同内容测试
(check-false (string=? "hello" "world"))
(check-false (string=? "abc" "def"))
(check-false (string=? "short" "longer"))

;; string=? 各种边界情况
(check-true (string=? "123" "123"))
(check-true (string=? "!@#$%" "!@#$%"))
(check-true (string=? "空格 测试" "空格 测试"))
(check-false (string=? "abc" "abcd"))
(check-false (string=? "abcd" "abc"))

;; string=? 特殊字符测试
(check-true (string=? "\n\t" "\n\t"))
(check-true (string=? "测试文本" "测试文本"))
(check-false (string=? "测试" "测试文本"))
(check-false (string=? "测试文本" "测试"))

;; string=? 空字符串测试
(check-true (string=? "" "" ""))
(check-true (string=? "a" "a" "a"))

;; string=? 多参数测试
(check-true (string=? "same" "same" "same"))
(check-false (string=? "same" "diff" "same"))
(check-false (string=? "one" "two" "three"))

;; string=? 二进制和Unicode字符串
(check-true (string=? "Hello, 世界!" "Hello, 世界!"))
(check-false (string=? "Hello, 世界!" "Hello, 世界! "))

;; 错误处理测试
(check-catch 'wrong-number-of-args (string=?))
(check-catch 'wrong-number-of-args (string=? "hello"))

#|
string-ci=?
按大小写不敏感的方式比较多个字符串是否相等。

语法
----
(string-ci=? string1 string2 ...)

参数
----
string1, string2, ... : string?
至少两个字符串参数。

返回值
------
boolean?
如果所有字符串在大小写不敏感情况下相等则返回 #t，否则返回 #f。

说明
----
1. 至少需要两个参数
2. 所有参数必须是字符串类型
3. 字符串比较不区分大小写（大小写等价）
4. 当所有字符串内容在不区分大小写情况下相同返回 #t
5. 返回布尔值结果

边界情况
--------
- 大小写不敏感：大写和小写字母被视为相同
- 空字符串比较：空字符串只与空字符串相等
- 特殊字符：所有字符需要内容相同，大小写不影响字母字符

错误处理
--------
wrong-number-of-args
当参数数量少于2个时抛出错误。
|#

;; string-ci=? 基本测试
(check-true (string-ci=? "hello" "hello"))
(check-true (string-ci=? "hello" "HELLO"))
(check-true (string-ci=? "Hello" "hello"))
(check-true (string-ci=? "" ""))
(check-true (string-ci=? "a" "A"))

;; string-ci=? 大小写不敏感测试
(check-true (string-ci=? "HELLO" "hello"))
(check-true (string-ci=? "Hello" "HELLO"))
(check-true (string-ci=? "aBc" "AbC"))
(check-true (string-ci=? "UPPER" "upper"))
(check-true (string-ci=? "Mixed" "mixed"))

;; string-ci=? 不同内容测试
(check-false (string-ci=? "hello" "HELLOWORLD"))
(check-false (string-ci=? "abc" "def"))
(check-false (string-ci=? "short" "longer"))
(check-false (string-ci=? "test" "TESTING"))

;; string-ci=? 各种边界情况
(check-true (string-ci=? "123" "123"))
(check-true (string-ci=? "!@#$%" "!@#$%"))
(check-true (string-ci=? "空格 测试" "空格 测试"))
(check-false (string-ci=? "abc" "abcd"))
(check-false (string-ci=? "abcd" "abc"))

;; string-ci=? 特殊字符测试
(check-true (string-ci=? "\n\t" "\n\t"))
(check-true (string-ci=? "测试文本" "测试文本"))
(check-false (string-ci=? "测试" "测试文本"))
(check-false (string-ci=? "测试文本" "测试"))

;; string-ci=? 空字符串测试
(check-true (string-ci=? "" "" ""))
(check-true (string-ci=? "a" "A" "a"))

;; string-ci=? 多参数测试
(check-true (string-ci=? "same" "SAME" "Same"))
(check-false (string-ci=? "same" "DIFF" "same"))
(check-false (string-ci=? "ONE" "two" "Three"))

;; string-ci=? 二进制和Unicode字符串
(check-true (string-ci=? "Hello, 世界!" "hello, 世界!"))
(check-true (string-ci=? "JAVA" "java" "Java"))
(check-false (string-ci=? "Hello" "HELLOWORLD"))

;; string-ci=? 大小写混合场景
(check-true (string-ci=? "Goldfish Scheme" "goldfish scheme"))
(check-true (string-ci=? "r7rs" "R7RS" "R7rs"))
(check-true (string-ci=? "CLaUdE" "claude"))
(check-false (string-ci=? "TeXMACS" "textmacs"))

;; 错误处理测试
(check-catch 'wrong-number-of-args (string-ci=?))
(check-catch 'wrong-number-of-args (string-ci=? "hello"))

#|
bytevector
返回一个新分配的字节向量，其元素包含传递给过程的所有参数。每个参数都必须是一个介于0到255之间的整数，表示字节向量中的一个字节。如果没有提供任何参数，将创建一个空的字节向量。

语法
----
(bytevector byte ...)

参数
----
byte... : integer?
零个或多个介于0到255之间的整数（包含边界），表示字节值。

返回值
------
bytevector?
新创建的字节向量，包含所有参数指定的字节值。

说明
----
1. 可以接受零个或多个参数
2. 每个参数必须在0-255的范围内
3. 无参数时创建空字节向量
4. 参数顺序就是字节向量中元素的顺序

错误处理
--------
wrong-type-arg
当任何参数不是在0-255范围内的整数时抛出错误。
|#

;; bytevector 基本测试
(check (bytevector) => #u8())
(check (bytevector 255) => #u8(255))
(check (bytevector 1 2 3 4) => #u8(1 2 3 4))
(check (bytevector 10 20 30 40 50) => #u8(10 20 30 40 50))

;; 边界测试
(check (bytevector 0) => #u8(0))
(check (bytevector 255) => #u8(255))
(check (bytevector 0 255) => #u8(0 255))

;; 不同长度测试
(check (bytevector) => #u8())
(check (bytevector 15) => #u8(15))
(check (bytevector 85 170) => #u8(85 170))
(check (bytevector 1 2 3 4 5 6 7 8 9 10) => #u8(1 2 3 4 5 6 7 8 9 10))

;; 错误处理测试
(check-catch 'wrong-type-arg (bytevector 256))
(check-catch 'wrong-type-arg (bytevector -1))
(check-catch 'wrong-type-arg (bytevector 123.0))
(check-catch 'wrong-type-arg (bytevector 123 #u8(1 2 3)))

#|
bytevector?
判断一个对象是否为字节向量类型的谓词。

语法
----
(bytevector? obj)

参数
----
obj : any?
任意对象。

返回值
------
boolean?
如果对象是一个字节向量，返回#t；否则返回#f。

说明
----
1. 用于检查对象是否为字节向量类型
2. #u8()形式创建的也是字节向量，即使为空
3. 能够正确识别所有类型的字节向量实例

错误处理
--------
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; bytevector? 基本测试
(check-true (bytevector? #u8()))
(check-true (bytevector? #u8(0)))
(check-true (bytevector? #u8(255)))
(check-true (bytevector? #u8(1 2 3 4 5)))
(check-true (bytevector? (bytevector)))
(check-true (bytevector? (bytevector 1 2 3)))

;; 类型判别测试
(check-true (bytevector? (bytevector 5 15 25)))
(check-false (bytevector? 123))
(check-false (bytevector? "hello"))
(check-false (bytevector? "list"))
(check-false (bytevector? 'symbol))

;; 错误处理测试
(check-catch 'wrong-number-of-args (bytevector?))
(check-catch 'wrong-number-of-args (bytevector? #u8(1 2 3) #u8(4 5 6)))

#|
make-bytevector
创建一个新的字节向量，指定长度和初始值为所有组成字节。

语法
----
(make-bytevector k [fill])

参数
----
k : integer?
必须是非负的精确整数，表示字节向量的长度。

fill : integer? 可选, 默认为0
0到255之间的整数，作为所有字节的初始值。

返回值
------
bytevector?
创建的字节向量，所有元素都设为指定的fill值。

说明
----
1. 可以指定长度和填充值
2. 填充值默认为0，如果提供必须在0-255范围内
3. 特殊字符如#等会被转换为对应的字节值

错误处理
--------
out-of-range
当k小于0时抛出错误。
wrong-type-arg
当任何参数不正确时抛出错误。
wrong-number-of-args
当参数数量不为1或2个时抛出错误。
|#

;; make-bytevector 基本测试
(check (make-bytevector 0) => #u8())
(check (make-bytevector 1) => #u8(0))
(check (make-bytevector 3) => #u8(0 0 0))
(check (make-bytevector 5 42) => #u8(42 42 42 42 42))
(check (make-bytevector 2 255) => #u8(255 255))

;; 不同长度测试
(check (make-bytevector 0 0) => #u8())
(check (make-bytevector 1 128) => #u8(128))
(check (make-bytevector 10 99) => #u8(99 99 99 99 99 99 99 99 99 99))

;; 边界条件测试
(check (make-bytevector 0) => #u8())
(check (make-bytevector 1 0) => #u8(0))
(check (make-bytevector 1 255) => #u8(255))

;; 特殊值测试
(check (make-bytevector 4 0) => #u8(0 0 0 0))
(check (make-bytevector 3 170) => #u8(170 170 170))
(check (make-bytevector 8 255) => #u8(255 255 255 255 255 255 255 255))

;; 错误处理测试
(check-catch 'out-of-range (make-bytevector -5))
(check-catch 'wrong-type-arg (make-bytevector 3 256))
(check-catch 'wrong-type-arg (make-bytevector 2 -1))
(check-catch 'wrong-type-arg (make-bytevector 3.5))
(check-catch 'wrong-type-arg (make-bytevector "hello"))
(check-catch 'wrong-number-of-args (make-bytevector))
(check-catch 'wrong-number-of-args (make-bytevector 1 2 3))



#|
bytevector-length
返回字节向量中的元素个数。

语法
----
(bytevector-length bv)

参数
----
bv : bytevector?
字节向量。

返回值
------
integer?
字节向量中的元素数量。

说明
----
1. 返回字节向量中的字节数
2. 空字节向量返回0
3. 结果是非负的精确整数

错误处理
--------
wrong-type-arg
当参数不是字节向量时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; bytevector-length 基本测试
(check (bytevector-length #u8()) => 0)
(check (bytevector-length #u8(1)) => 1)
(check (bytevector-length #u8(1 2 3)) => 3)
(check (bytevector-length #u8(255)) => 1)
(check (bytevector-length #u8(1 2 3 4 5 6 7 8 9 10)) => 10)

;; 使用不同类型创建的字节向量测试
(check (bytevector-length (bytevector)) => 0)
(check (bytevector-length (bytevector 50 150 250)) => 3)
(check (bytevector-length (make-bytevector 5 42)) => 5)
(check (bytevector-length (make-bytevector 10 0)) => 10)
(check (bytevector-length (make-bytevector 0)) => 0)
(check (bytevector-length "hello") => 5)
(check (bytevector-length 123) => #f)
(check (bytevector-length 'symbol) => #f)

;; 错误处理测试

(check-catch 'wrong-number-of-args (bytevector-length))
(check-catch 'wrong-number-of-args (bytevector-length #u8(1 2 3) #u8(4 5)))

#|
bytevector-u8-ref
返回字节向量中指定索引位置的字节值。

语法
----
(bytevector-u8-ref bv k)

参数
----
bv : bytevector?
字节向量。

k : integer?
非负的精确整数，表示字节索引位置，必须小于字节向量的长度。

返回值
------
integer?
位置k处的字节值，是一个0到255之间的整数。

说明
----
1. 用0基索引访问字节向量中的元素
2. 索引必须是从0到长度减1的非负整数
3. 返回对应位置的字节值

错误处理
--------
type-error
当bv不是字节向量时或k不是整数时抛出错误。
out-of-range
当k小于0或大于等于字节向量长度时抛出错误。
|#

;; bytevector-u8-ref 基本测试
(check (bytevector-u8-ref #u8(5 15 25) 0) => 5)
(check (bytevector-u8-ref #u8(5 15 25) 1) => 15)
(check (bytevector-u8-ref #u8(5 15 25) 2) => 25)
(check (bytevector-u8-ref #u8(255) 0) => 255)
(check (bytevector-u8-ref #u8(0) 0) => 0)


;; 使用其他函数创建的字节向量测试
(check (bytevector-u8-ref (bytevector 10 20 30 40) 0) => 10)
(check (bytevector-u8-ref (bytevector 10 20 30 40) 1) => 20)
(check (bytevector-u8-ref (bytevector 10 20 30 40) 3) => 40)
(check (bytevector-u8-ref (bytevector 200 150 100 50) 2) => 100)

(check (bytevector-u8-ref (make-bytevector 4 99) 0) => 99)
(check (bytevector-u8-ref (make-bytevector 4 99) 3) => 99)
(check (bytevector-u8-ref #u8(1) 0) => 1)  

;; 复杂字节向量测试
(check (bytevector-u8-ref #u8(10 20 30 40 50 60 70 80 90 100) 9) => 100)
(check (bytevector-u8-ref #u8(128 64 32 16 8 4 2 1) 4) => 8)

;; UTF-8转换测试
(check (bytevector-u8-ref (string->utf8 "XYZ") 0) => 88) ;; ASCII 'X'
(check (bytevector-u8-ref (string->utf8 "XYZ") 1) => 89) ;; ASCII 'Y'
(check (bytevector-u8-ref (string->utf8 "A") 0) => 65)

;; 错误处理测试

(check-catch 'wrong-type-arg (bytevector-u8-ref 123 0))
(check-catch 'wrong-type-arg (bytevector-u8-ref "hello" 0))
(check-catch 'wrong-type-arg (bytevector-u8-ref #u8(1 2 3) 1.5))
(check-catch 'out-of-range (bytevector-u8-ref #u8() 0)) ;; empty case
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) -1))
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) 3))
(check-catch 'out-of-range (bytevector-u8-ref #u8(1 2 3) 1 3))
(check-catch 'out-of-range (bytevector-u8-ref #u8() 0))
(check-catch 'wrong-number-of-args (bytevector-u8-ref #u8(1 2 3)))


#|
bytevector-u8-set!
修改字节向量中指定位置的字节值。

语法
----
(bytevector-u8-set! bv k byte)

参数
----
bv : bytevector?
要修改的字节向量。

k : integer?
非负的精确整数，表示字节索引位置，必须小于字节向量的长度。

byte : integer?
0到255之间的整数，表示要设置的新的字节值。

返回值
------
unspecified
过程修改字节向量后立即返回，没有特定的返回值。

错误处理
--------
out-of-range
当k小于0或大于等于字节向量长度时抛出错误。
wrong-type-arg
当byte不是0-255之间的整数时抛出错误。
|#

(let1 bv (bytevector 1 2 3 4 5)
  (bytevector-u8-set! bv 1 4)
  (check bv => #u8(1 4 3 4 5))
  (bytevector-u8-set! bv 0 10)
  (check bv => #u8(10 4 3 4 5))
  (bytevector-u8-set! bv 4 255)
  (check bv => #u8(10 4 3 4 255)) 
) ;let1

(let1 bv (bytevector 5)
  (bytevector-u8-set! bv 0 10)
  (check bv => #u8(10))  
) ;let1


;; 错误处理测试
(check-catch 'out-of-range (bytevector-u8-set! #u8() 0 5))
(check-catch 'out-of-range (bytevector-u8-set! #u8(1 2 3) -1 5))
(check-catch 'out-of-range (bytevector-u8-set! #u8(1 2 3) 3 5))
(check-catch 'wrong-type-arg (bytevector-u8-set! 123 0 5))
(check-catch 'wrong-type-arg (bytevector-u8-set! "hello" 0 5))
(check-catch 'wrong-type-arg (bytevector-u8-set! #u8(1 2 3) 1 256))
(check-catch 'wrong-type-arg (bytevector-u8-set! #u8(1 2 3) 1 -1))

#|
bytevector-copy
创建一个新的字节向量，它是现有字节向量的完整或部分副本。

语法
----
(bytevector-copy bv [start [end]])

参数
----
bv : bytevector?
要被复制的源字节向量。

start : integer? 可选, 默认为0
非负的精确整数，表示复制开始的索引位置，必须小于bv的长度。

end : integer? 可选, 默认为(bytevector-length bv)
非负的精确整数，表示复制结束的索引位置（不包括该位置），必须大于等于start且小于等于bv的长度。

返回值
------
bytevector?
新的字节向量，包含从start到end-1位置的元素副本。

错误处理
--------
wrong-type-arg
当bv不是字节向量抛出错误。

out-of-range
当start或end超出有效范围时抛出错误。

wrong-number-of-args
参数数量不正确时抛出错误。
|#

;; bytevector-copy 基本测试
(check (bytevector-copy #u8()) => #u8())
(check (bytevector-copy #u8(1 2 3)) => #u8(1 2 3))
(check (bytevector-copy #u8(255 0 128)) => #u8(255 0 128))

;; 段复制测试
(check (bytevector-copy #u8(1 2 3 4 5) 0 3) => #u8(1 2 3))
(check (bytevector-copy #u8(1 2 3 4 5) 1 4) => #u8(2 3 4))
(check (bytevector-copy #u8(1 2 3 4 5) 2) => #u8(3 4 5))

;; 边界测试
(check (bytevector-copy #u8(50 100 150) 0 0) => #u8())
(check (bytevector-copy #u8(50 100 150) 0 1) => #u8(50))
(check (bytevector-copy #u8(50 100 150) 2 3) => #u8(150))

;; 完整范围
(check (bytevector-copy #u8(10 20 30 40 50) 0 5) => #u8(10 20 30 40 50))

;; 独立对象测试
(let1 bv (bytevector 1 2 3 4 5)
  (check (bytevector-copy bv 1 4) => #u8(2 3 4))
) ;let1

;; 错误处理
(check-catch 'wrong-type-arg (bytevector-copy 123))
(check-catch 'wrong-type-arg (bytevector-copy "hello"))
(check-catch 'out-of-range (bytevector-copy #u8(1 2 3) -1))
(check-catch 'out-of-range (bytevector-copy #u8(1 2 3) 4))
(check-catch 'out-of-range (bytevector-copy #u8(1 2 3) 0 5))
(check-catch 'out-of-range (bytevector-copy #u8(1 2 3) 2 1))


(check (bytevector-append #u8() #u8()) => #u8())
(check (bytevector-append #u8() #u8(1)) => #u8(1))
(check (bytevector-append #u8(1) #u8()) => #u8(1))



#|
open-input-string
将一个字符串转换为输入端口

语法
----
(open-input-string string)

参数
----
string : string?
一个字符串对象

返回值
-----
port
一个文本输入端口，该端口会从给定的字符串中读取字符。
注意：如果在端口使用期间修改了原始字符串，其行为是未定义的。

|#

;; eof on empty
(let1 port (open-input-string "")
  (check (eof-object? (read-char port)) => #t)
) ;let1

;; read-char
(let1 port (open-input-string "abc")
  (check (read-char port) => #\a)
  (check (read-char port) => #\b)
  (check (read-char port) => #\c)
  (check (eof-object? (read-char port)) => #t)
) ;let1

;; read-char, Unicode (Not Support)
(let1 port (open-input-string "λμ") ; #\x03bb #\x03bc
  (check (read-char port) => #\xce)
  (check (read-char port) => #\xbb)
  (check (read-char port) => #\xce)
  (check (read-char port) => #\xbc)
) ;let1

;; read-string, Unicode
(let1 port (open-input-string "λμ")
  (check (read-string 2 port) => "λ")
  (check (read-string 2 port) => "μ")
) ;let1

#|
open-output-string
创建一个字符串输出端口用于累积字符

语法
----
(open-output-string)

返回值
-----
port
返回一个新的文本输出端口，所有写入该端口的字符会被累积，
可通过 get-output-string 函数获取累积的字符串。

|#

;; empty
(let1 port (open-output-string)
  (check (get-output-string port) => "")
) ;let1

(let1 port (open-output-string)
  (display "abc" port)
  (check (get-output-string port) => "abc")
) ;let1

(let1 port (open-output-string)
  (display "λμ" port)
  (check (get-output-string port) => "λμ")
) ;let1

#|
get-output-string
获取输出端口累积的字符串

语法
----
(get-output-string port)

参数
----
port : port?
必须是由 open-output-string 创建的输出端口

返回值
-----
string?
返回一个字符串，包含按输出顺序累积到端口的所有字符。
注意：如果修改返回的字符串，其行为是未定义的。

错误
----
wrong-type-arg
如果 port 参数不是由 open-output-string 创建的端口，抛出错误。
|#

(let1 port (open-output-string)
  (display "xyz" port)
  (check (get-output-string port) => "xyz")
) ;let1

(let1 port (open-input-string "ERROR")
  (check-catch 'wrong-type-arg (get-output-string port))
) ;let1

#|
read
从输入端口读取一个S表达式，根据R7RS规范，read函数用于从给定的输入端口读取Scheme数据。

语法
----
(read)
(read port)

参数
----
port : port? (可选)
输入端口，如果未提供则使用当前输入端口

返回值
-----
any?
返回从端口读取的S表达式，到达文件末尾时返回EOF对象

描述
----
read函数从指定的输入端口读取一个完整的S表达式。如果未提供端口参数，
则从当前输入端口读取。读取过程会正确处理各种Scheme数据类型，包括
基本类型（数字、字符串、符号、布尔值）和复合类型（列表、向量等）。

行为特征
------
- 读取完整的S表达式，包括嵌套结构
- 正确处理引号、反引号等特殊语法
- 支持所有Scheme数据类型的读取
- 到达文件末尾时返回EOF对象
- 输入格式错误会抛出读取错误

错误处理
------
- 输入格式错误：抛出读取错误
- 端口错误：如果端口无效会抛出相应错误
- 内存不足：可能抛出内存错误

与write的关系
------------
read函数与write函数配合使用可以实现数据的序列化和反序列化：
(write data port) 写入的数据可以通过 (read port) 重新读取

跨平台行为
---------
- 字符编码：支持UTF-8编码的文本输入
- 数字格式：支持各种数字字面量格式
- 字符串转义：正确处理转义字符
|#

;; 基本数据类型读取测试
(let1 port (open-input-string "123")
  (check (read port) => 123)
) ;let1

(let1 port (open-input-string "-456")
  (check (read port) => -456)
) ;let1

(let1 port (open-input-string "3.14")
  (check (read port) => 3.14)
) ;let1

(let1 port (open-input-string "\"hello world\"")
  (check (read port) => "hello world")
) ;let1

(let1 port (open-input-string "hello")
  (check (read port) => 'hello)
) ;let1

(let1 port (open-input-string "#t")
  (check (read port) => #t)
) ;let1

(let1 port (open-input-string "#f")
  (check (read port) => #f)
) ;let1

;; 列表读取测试
(let1 port (open-input-string "(1 2 3)")
  (check (read port) => '(1 2 3))
) ;let1

(let1 port (open-input-string "(a b c)")
  (check (read port) => '(a b c))
) ;let1

(let1 port (open-input-string "(1 \"two\" 3)")
  (check (read port) => '(1 "two" 3))
) ;let1

;; 嵌套列表读取测试
(let1 port (open-input-string "(1 (2 3) 4)")
  (check (read port) => '(1 (2 3) 4))
) ;let1

(let1 port (open-input-string "((a b) (c d))")
  (check (read port) => '((a b) (c d)))
) ;let1

;; 向量读取测试
(let1 port (open-input-string "#(1 2 3)")
  (check (read port) => #(1 2 3))
) ;let1

(let1 port (open-input-string "#(a \"b\" c)")
  (check (read port) => #(a "b" c))
) ;let1

;; 引号语法测试
(let1 port (open-input-string "'hello")
  (check (read port) => ''hello)
) ;let1

(let1 port (open-input-string "'(1 2 3)")
  (check (read port) => ''(1 2 3))
) ;let1

(let1 port (open-input-string "`hello")
  (check (read port) => '`hello)
) ;let1

;; 取消引号语法 - 这些在当前实现中可能不支持
;; (let1 port (open-input-string ",hello")
;;   (check (read port) => ',hello))

;; (let1 port (open-input-string ",@hello")
;;   (check (read port) => ',@hello))

;; 复杂表达式测试
(let1 port (open-input-string "(+ 1 2 3)")
  (check (read port) => '(+ 1 2 3))
) ;let1

(let1 port (open-input-string "(define x 42)")
  (check (read port) => '(define x 42))
) ;let1

(let1 port (open-input-string "(if #t yes no)")
  (check (read port) => '(if #t yes no))
) ;let1

;; 空列表测试
(let1 port (open-input-string "()")
  (check (read port) => '())
) ;let1

;; 布尔值列表测试
(let1 port (open-input-string "(#t #f #t)")
  (check (read port) => '(#t #f #t))
) ;let1

;; 混合类型列表测试
(let1 port (open-input-string "(1 \"two\" 'three 4.0)")
  (check (read port) => '(1 "two" 'three 4.0))
) ;let1

;; 文件结束测试
(let1 port (open-input-string "")
  (check (eof-object? (read port)) => #t)
) ;let1

(let1 port (open-input-string "123")
  (check (read port) => 123)
  (check (eof-object? (read port)) => #t)
) ;let1

;; 多个表达式测试
(let1 port (open-input-string "123 456 \"hello\"")
  (check (read port) => 123)
  (check (read port) => 456)
  (check (read port) => "hello")
  (check (eof-object? (read port)) => #t)
) ;let1

;; 注释处理测试（如果支持）
(let1 port (open-input-string "123 ; this is a comment\n456")
  (check (read port) => 123)
  (check (read port) => 456)
) ;let1

;; 空白字符处理测试
(let1 port (open-input-string "   123   456   ")
  (check (read port) => 123)
  (check (read port) => 456)
) ;let1

;; 换行符处理测试
(let1 port (open-input-string "123\n456\n789")
  (check (read port) => 123)
  (check (read port) => 456)
  (check (read port) => 789)
) ;let1

;; 与write联动测试
(let1 output-port (open-output-string)
  (write '(1 2 3) output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => '(1 2 3))
  ) ;let1
) ;let1

(let1 output-port (open-output-string)
  (write "hello world" output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => "hello world")
  ) ;let1
) ;let1

(let1 output-port (open-output-string)
  (write 123.456 output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => 123.456)
  ) ;let1
) ;let1

;; 错误处理测试 - 不完整的表达式
;; (let1 port (open-input-string "(1 2")
;;   (check-catch 'read-error (read port)))

;; 大数字测试
(let1 port (open-input-string "12345678901234567890")
  (check-true (number? (read port)))
) ;let1

;; 特殊符号测试
(let1 port (open-input-string "hello-world hello_world hello.world")
  (check (read port) => 'hello-world)
  (check (read port) => 'hello_world)
  (check (read port) => 'hello.world)
) ;let1

;; 中文符号测试
(let1 port (open-input-string "'中文测试")
  (check (read port) => ''中文测试)
) ;let1

;; 嵌套引号测试
(let1 port (open-input-string "''hello")
  (check (read port) => '''hello)
) ;let1

;; 复杂嵌套测试
(let1 port (open-input-string "(a (b (c d)) e)")
  (check (read port) => '(a (b (c d)) e))
) ;let1

;; 向量嵌套测试
(let1 port (open-input-string "#(1 #(2 3) 4)")
  (check (read port) => #(1 #(2 3) 4))
) ;let1

;; 当前输入端口测试（需要重定向）
;; (let1 original-input (current-input-port)
;;   (let1 string-port (open-input-string "42")
;;     (set-current-input-port! string-port)
;;     (check (read) => 42)
;;     (set-current-input-port! original-input)))

#|
write
向输出端口写入一个S表达式，根据R7RS规范，write函数用于将Scheme数据写入给定的输出端口。

语法
----
(write obj)
(write obj port)

参数
----
obj : any?
要写入的Scheme对象
port : port? (可选)
输出端口，如果未提供则使用当前输出端口

返回值
-----
unspecified
返回值未指定，主要作用是副作用（向端口写入数据）

描述
----
write函数将指定的Scheme对象以可读格式写入输出端口。如果未提供端口参数，
则写入当前输出端口。输出的格式应该能被read函数正确读取，实现数据的
序列化和反序列化。

行为特征
------
- 输出完整的S表达式，包括嵌套结构
- 正确处理引号、反引号等特殊语法
- 支持所有Scheme数据类型的写入
- 字符串中的特殊字符会被正确转义
- 输出的格式注重可读性而非美观性

与display的区别
--------------
- write: 注重数据的可读性，输出格式能被read重新读取
- display: 注重人类可读性，输出更简洁的格式
- 例如：write输出字符串带引号，display可能不带

与read的关系
------------
write函数与read函数配合使用可以实现数据的序列化和反序列化：
(write data port) 写入的数据可以通过 (read port) 重新读取

错误处理
------
- 端口错误：如果端口无效会抛出相应错误
- IO错误：包括磁盘满、权限不足等
- 循环结构：需要特殊处理以避免无限递归

跨平台行为
---------
- 字符编码：支持UTF-8编码的文本输出
- 数字格式：输出标准的数字字面量格式
- 字符串转义：正确处理需要转义的字符
|#

;; 基本数据类型写入测试
(let1 port (open-output-string)
  (write 123 port)
  (check (get-output-string port) => "123")
) ;let1

(let1 port (open-output-string)
  (write -456 port)
  (check (get-output-string port) => "-456")
) ;let1

(let1 port (open-output-string)
  (write 3.14 port)
  (check (get-output-string port) => "3.14")
) ;let1

(let1 port (open-output-string)
  (write "hello world" port)
  (check (get-output-string port) => "\"hello world\"")
) ;let1

(let1 port (open-output-string)
  (write 'hello port)
  (check (get-output-string port) => "hello")
) ;let1

(let1 port (open-output-string)
  (write #t port)
  (check (get-output-string port) => "#t")
) ;let1

(let1 port (open-output-string)
  (write #f port)
  (check (get-output-string port) => "#f")
) ;let1

;; 列表写入测试
(let1 port (open-output-string)
  (write '(1 2 3) port)
  (check (get-output-string port) => "(1 2 3)")
) ;let1

(let1 port (open-output-string)
  (write '(a b c) port)
  (check (get-output-string port) => "(a b c)")
) ;let1

(let1 port (open-output-string)
  (write '(1 "two" 3) port)
  (check (get-output-string port) => "(1 \"two\" 3)")
) ;let1

;; 嵌套列表写入测试
(let1 port (open-output-string)
  (write '(1 (2 3) 4) port)
  (check (get-output-string port) => "(1 (2 3) 4)")
) ;let1

(let1 port (open-output-string)
  (write '((a b) (c d)) port)
  (check (get-output-string port) => "((a b) (c d))")
) ;let1

;; 向量写入测试
(let1 port (open-output-string)
  (write #(1 2 3) port)
  (check (get-output-string port) => "#(1 2 3)")
) ;let1

(let1 port (open-output-string)
  (write #(a "b" c) port)
  (check (get-output-string port) => "#(a \"b\" c)")
) ;let1

;; 引号语法写入测试
(let1 port (open-output-string)
  (write ''hello port)
  (check (get-output-string port) => "'hello")
) ;let1

(let1 port (open-output-string)
  (write ''(1 2 3) port)
  (check (get-output-string port) => "'(1 2 3)")
) ;let1

(let1 port (open-output-string)
  (write '`hello port)
  (check (get-output-string port) => "'hello")
) ;let1

;; 复杂表达式写入测试
(let1 port (open-output-string)
  (write '(+ 1 2 3) port)
  (check (get-output-string port) => "(+ 1 2 3)")
) ;let1

(let1 port (open-output-string)
  (write '(define x 42) port)
  (check (get-output-string port) => "(define x 42)")
) ;let1

(let1 port (open-output-string)
  (write '(if #t yes no) port)
  (check (get-output-string port) => "(if #t yes no)")
) ;let1

;; 空列表写入测试
(let1 port (open-output-string)
  (write '() port)
  (check (get-output-string port) => "()")
) ;let1

;; 布尔值列表写入测试
(let1 port (open-output-string)
  (write '(#t #f #t) port)
  (check (get-output-string port) => "(#t #f #t)")
) ;let1

;; 混合类型列表写入测试
(let1 port (open-output-string)
  (write '(1 "two" 'three 4.0) port)
  (check (get-output-string port) => "(1 \"two\" 'three 4.0)")
) ;let1

;; 字符串转义测试
(let1 port (open-output-string)
  (write "hello\nworld" port)
  (check (get-output-string port) => "\"hello\\nworld\"")
) ;let1

(let1 port (open-output-string)
  (write "hello\tworld" port)
  (check (get-output-string port) => "\"hello\\tworld\"")
) ;let1

(let1 port (open-output-string)
  (write "hello\"world" port)
  (check (get-output-string port) => "\"hello\\\"world\"")
) ;let1

(let1 port (open-output-string)
  (write "hello\\world" port)
  (check (get-output-string port) => "\"hello\\\\world\"")
) ;let1

;; 特殊字符测试
(let1 port (open-output-string)
  (write "\x00;\x01;\x02;" port)
  (check (get-output-string port) => "\"\\x00;\\x01;\\x02;\"")
) ;let1

;; 中文字符串写入测试
(let1 port (open-output-string)
  (write "你好世界" port)
  (check (get-output-string port) => "\"你好世界\"")
) ;let1

;; 大数字写入测试
(let1 port (open-output-string)
  (write 12345678901234567890 port)
  (check-true (string? (get-output-string port)))
) ;let1

;; 分数写入测试
(let1 port (open-output-string)
  (write 1/2 port)
  (check (get-output-string port) => "1/2")
) ;let1

(let1 port (open-output-string)
  (write -22/7 port)
  (check (get-output-string port) => "-22/7")
) ;let1

;; 复数写入测试
(let1 port (open-output-string)
  (write 1+2i port)
  (check (get-output-string port) => "1.0+2.0i")
) ;let1

(let1 port (open-output-string)
  (write 3.14-2.71i port)
  (check (get-output-string port) => "3.14-2.71i")
) ;let1

;; 嵌套向量测试
(let1 port (open-output-string)
  (write #(1 #(2 3) 4) port)
  (check (get-output-string port) => "#(1 #(2 3) 4)")
) ;let1

;; 深层嵌套测试
(let1 port (open-output-string)
  (write '(a (b (c d)) e) port)
  (check (get-output-string port) => "(a (b (c d)) e)")
) ;let1

;; 与read联动测试 - 确保write的输出能被read正确读取
(let1 output-port (open-output-string)
  (write '(1 2 3) output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => '(1 2 3))
  ) ;let1
) ;let1

(let1 output-port (open-output-string)
  (write "hello world" output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => "hello world")
  ) ;let1
) ;let1

(let1 output-port (open-output-string)
  (write 123.456 output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => 123.456)
  ) ;let1
) ;let1

(let1 output-port (open-output-string)
  (write #(#t #f "hello") output-port)
  (let1 input-port (open-input-string (get-output-string output-port))
    (check (read input-port) => #(#t #f "hello"))
  ) ;let1
) ;let1

;; 多个值写入测试
(let1 port (open-output-string)
  (write 123 port)
  (write 456 port)
  (write "hello" port)
  (check (get-output-string port) => "123456\"hello\"")
) ;let1

;; 当前输出端口测试（需要重定向）
;; (let1 original-output (current-output-port)
;;   (let1 string-port (open-output-string)
;;     (set-current-output-port! string-port)
;;     (write 42)
;;     (check (get-output-string string-port) => "42")
;;     (set-current-output-port! original-output)))

;; 特殊符号写入测试
(let1 port (open-output-string)
  (write 'hello-world port)
  (check (get-output-string port) => "hello-world")
) ;let1

(let1 port (open-output-string)
  (write 'hello_world port)
  (check (get-output-string port) => "hello_world")
) ;let1

(let1 port (open-output-string)
  (write 'hello.world port)
  (check (get-output-string port) => "hello.world")
) ;let1

;; 中文符号写入测试
(let1 port (open-output-string)
  (write '中文测试 port)
  (check (get-output-string port) => "中文测试")
) ;let1

;; 嵌套引号写入测试
(let1 port (open-output-string)
  (write '''hello port)
  (check (get-output-string port) => "''hello")
) ;let1

;; 空字符串测试
(let1 port (open-output-string)
  (write "" port)
  (check (get-output-string port) => "\"\"")
) ;let1

;; 单字符字符串测试
(let1 port (open-output-string)
  (write "a" port)
  (check (get-output-string port) => "\"a\"")
) ;let1

;; 数值边界测试
(let1 port (open-output-string)
  (write 0 port)
  (check (get-output-string port) => "0")
) ;let1

(let1 port (open-output-string)
  (write -0.0 port)
  (check (get-output-string port) => "-0.0")
) ;let1

;; 精确与非精确数测试
(let1 port (open-output-string)
  (write 42 port)
  (check (get-output-string port) => "42")
) ;let1

(let1 port (open-output-string)
  (write 42.0 port)
  (check (get-output-string port) => "42.0")
) ;let1

;; 复杂嵌套结构测试
(let1 port (open-output-string)
  (write '((1 2) (3 4) (5 (6 7))) port)
  (check (get-output-string port) => "((1 2) (3 4) (5 (6 7)))")
) ;let1

;; 向量和列表混合测试
(let1 port (open-output-string)
  (write '(#(1 2) #(3 4) 5) port)
  (check (get-output-string port) => "(#(1 2) #(3 4) 5)")
) ;let1

;; 长列表测试
(let1 port (open-output-string)
  (write '(1 2 3 4 5 6 7 8 9 10) port)
  (check (get-output-string port) => "(1 2 3 4 5 6 7 8 9 10)")
) ;let1

;; 多维向量测试
(let1 port (open-output-string)
  (write #(#(1 2) #(3 4)) port)
  (check (get-output-string port) => "#(#(1 2) #(3 4))")
) ;let1

(check-report)
