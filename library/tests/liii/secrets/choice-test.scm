;; choice
;; 从非空列表或向量中密码学安全地随机选取单一元素。
;;
;; 语法
;; ----
;; (choice seq)
;;
;; 参数
;; ----
;; seq : list 或 vector
;; 非空的列表或向量。
;;
;; 返回值
;; -----
;; 任意对象
;; seq 中的某个元素。
;;
;; 说明
;; ----
;; 1. 选取下标由 randbelow 生成，因此不存在模偏差。
;; 2. 空序列抛出 value-error，非列表/向量类型抛出 type-error。
;;
;; 错误处理
;; ----
;; seq 不是列表或向量时抛出 type-error；seq 为空时抛出 value-error。

(import (liii check) (liii secrets) (liii string))

(check-set-mode! 'report-failed)

(define (contains? x lst)
  (if (member x lst) #t #f)
) ;define

;; 从列表中选取
(check (contains? (choice '(1 2 3 4 5)) '(1 2 3 4 5)) => #t)
(check (contains? (choice (list "a" "b" "c")) '("a" "b" "c")) => #t)

;; 单元素列表总是返回该元素
(check (choice '(42)) => 42)

;; 从向量中选取
(check (contains? (choice (vector 1 2 3 4 5)) '(1 2 3 4 5)) => #t)
(check (choice (vector 42)) => 42)

;; 列表与向量中的所有元素都应能被取到
(let loop
  ((i 0) (seen '()))
  (if (>= i 500)
    (check (= (length seen) 5) => #t)
    (let ((v (choice '(1 2 3 4 5))))
      (loop (+ i 1) (if (member v seen) seen (cons v seen)))
    ) ;let
  ) ;if
) ;let

(let loop
  ((i 0) (seen '()))
  (if (>= i 500)
    (check (= (length seen) 5) => #t)
    (let ((v (choice (vector 1 2 3 4 5))))
      (loop (+ i 1) (if (member v seen) seen (cons v seen)))
    ) ;let
  ) ;if
) ;let

;; 空序列抛 value-error
(check-catch 'value-error (choice '()))
(check-catch 'value-error (choice (vector)))

;; 类型错误抛 type-error
(check-catch 'type-error (choice 42))
(check-catch 'type-error (choice "abc"))
(check-catch 'type-error (choice #t))

(check-report)
