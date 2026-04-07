(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-for-each
;; 遍历 flexvector 每个元素，用于副作用。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-for-each proc fv)
;; (flexvector-for-each proc fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;;   接受一个或多个参数的函数。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回值未指定（通常是未定义值）。
;;
;; 副作用
;; -----
;; 对每个元素调用 proc。
;;
;; 另见
;; ----
;; flexvector-for-each/index - 带索引遍历

;; 基本遍历：收集元素
(let ((fv (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
  (check res => '(30 20 10))
) ;let

;; 遍历顺序是索引顺序
(let ((fv (flexvector 'a 'b 'c))
      (res '()))
  (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
  (check res => '(c b a))
) ;let

;; 空向量（不执行）
(let ((count 0))
  (flexvector-for-each (lambda (x) (set! count (+ count 1))) (flexvector))
  (check count => 0)
) ;let

;; 单元素
(let ((fv (flexvector 'only))
      (res #f))
  (flexvector-for-each (lambda (x) (set! res x)) fv)
  (check res => 'only)
) ;let

;; 多向量遍历（同时遍历多个向量）
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30))
      (sums '()))
  (flexvector-for-each (lambda (x y) (set! sums (cons (+ x y) sums)))
                       fv1 fv2
  ) ;flexvector-for-each
  (check sums => '(33 22 11))
) ;let

;; 多向量长度不同取最短
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20))
      (count 0))
  (flexvector-for-each (lambda (x y) (set! count (+ count 1)))
                       fv1 fv2
  ) ;flexvector-for-each
  (check count => 2)
) ;let

;; 三向量遍历
(let ((fv1 (flexvector 1 2))
      (fv2 (flexvector 10 20))
      (fv3 (flexvector 100 200))
      (res '()))
  (flexvector-for-each (lambda (x y z) (set! res (cons (+ x y z) res)))
                       fv1 fv2 fv3
  ) ;flexvector-for-each
  (check res => '(222 111))
) ;let

(check-report)
