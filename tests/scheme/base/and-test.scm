(import (liii check))
(import (liii base))
(import (scheme base))

(check-set-mode! 'report-failed)

;; and
;; 对任意数量的参数执行逻辑与操作，支持短路求值。
;;
;; 语法
;; ----
;; (and [expr ...])
;;
;; 参数
;; ----
;; expr : any
;; 任意类型的表达式。在 Scheme 中，除了 #f 之外的所有值都被视为真值。
;;
;; 返回值
;; -----
;; any
;; 如果没有任何表达式，返回 #t
;; 如果只有一个表达式，返回该表达式的结果
;; 对于多个表达式，返回最后一个真值表达式的结果，或者遇到第一个假值时立即返回 #f
;;
;; 短路求值
;; -------
;; 从左到右依次求值，一旦遇到 #f 就立即停止求值并返回 #f

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
  (let ((vec (make-vector 5)))
    (do ((i 0 (+ i 1)))
        ((= i 5) vec)
        (vector-set! vec i i)
    ) ;do
  ) ;let
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

(check-report)
