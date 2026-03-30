(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define comp (bag-comparator b-empty))

#|
bag-unfold
使用 unfold 模式创建 bag。

语法
----
(bag-unfold stop? mapper successor seed comparator)

参数
----
stop? : procedure
停止谓词。接收当前种子，返回布尔值。

mapper : procedure
映射函数。接收当前种子，返回要添加到 bag 的元素。

successor : procedure
后继函数。接收当前种子，返回下一个种子。

seed : any
初始种子。

comparator : comparator
比较器。

返回值
-----
返回由 unfold 生成的 bag。
|#
(define b-unfold
  (bag-unfold (lambda (n) (> n 3))
              (lambda (n) n)
              (lambda (n) (+ n 1))
              1
              comp))
(check (bag-member b-unfold 1 #f) => 1)
(check (bag-member b-unfold 2 #f) => 2)
(check (bag-member b-unfold 3 #f) => 3)
(check (bag-member b-unfold 4 'no) => 'no)
(check-true (eq? (bag-comparator b-unfold) comp))
(check-catch 'type-error
             (bag-unfold (lambda (n) #t)
                         (lambda (n) n)
                         (lambda (n) n)
                         0
                         "not a comparator"))

;; stop? 立即为真，返回空 bag
(define b-unfold-empty
  (bag-unfold (lambda (n) #t)
              (lambda (n) n)
              (lambda (n) n)
              0
              comp))
(check (bag-member b-unfold-empty 1 'none) => 'none)

;; mapper 返回常量，重复元素也应能命中
(define b-unfold-dup
  (bag-unfold (lambda (n) (> n 2))
              (lambda (n) 'x)
              (lambda (n) (+ n 1))
              0
              comp))
(check (bag-member b-unfold-dup 'x #f) => 'x)

(check-report)
