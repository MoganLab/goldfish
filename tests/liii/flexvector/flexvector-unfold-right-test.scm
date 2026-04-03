(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-unfold-right
;; 通过右展开构造 flexvector。结果与 unfold 相同，但元素顺序相反。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-unfold-right pred? gen succ seed)
;; (flexvector-unfold-right pred? gen succ seed1 seed2 ...)
;;
;; 参数
;; ----
;; pred? : procedure
;;   (pred? state) 当返回 #t 时停止。
;;
;; gen : procedure
;;   (gen state) 返回当前元素。
;;
;; succ : procedure
;;   (succ state) 返回下一个状态。
;;
;; seed : any
;;   初始状态。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，元素顺序与 unfold 相反。
;;
;; 另见
;; ----
;; flexvector-unfold - 从左展开

;; 基本右展开：结果与 unfold 相反
(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) (> x 10))
                                  (lambda (x) (* x x))
                                  (lambda (x) (+ x 1))
                                  1))
       => #(100 81 64 49 36 25 16 9 4 1))

;; 与 unfold 对比
(let* ((unfold-result (flexvector->vector
                        (flexvector-unfold (lambda (x) (> x 5))
                                           (lambda (x) x)
                                           (lambda (x) (+ x 1))
                                           1)))
       (unfold-right-result (flexvector->vector
                              (flexvector-unfold-right (lambda (x) (> x 5))
                                                       (lambda (x) x)
                                                       (lambda (x) (+ x 1))
                                                       1))))
  (check unfold-result => #(1 2 3 4 5))
  (check unfold-right-result => #(5 4 3 2 1)))

;; 递减序列
(let ((result (flexvector->list
                (flexvector-unfold-right (lambda (n) (= n 0))
                                         (lambda (n) n)
                                         (lambda (n) (- n 1))
                                         5))))
  (check result => '(0 1 2 3 4 5)))

;; 空结果
(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) #t)
                                  (lambda (x) x)
                                  (lambda (x) x)
                                  'seed))
       => #())

;; 单元素
(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) (> x 0))
                                  (lambda (x) x)
                                  (lambda (x) (+ x 1))
                                  0))
       => #(0))

(check-report)
