(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-fold-right
;; 从右到左折叠（右折叠）。时间复杂度 O(n)。
;;
;; 注意：SRFI-214 的 fold-right 顺序与 SRFI-1 列表 fold-right 不同，
;; 其累积器在参数第一位: (proc acc x) 而非 (proc x acc)
;;
;; 语法
;; ----
;; (flexvector-fold-right proc nil fv)
;; (flexvector-fold-right proc nil fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;;   (proc accumulator element) 或 (proc accumulator e1 e2 ...)
;;   返回新的累积值。
;;
;; nil : any
;;   初始累积值。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回最终的累积值。
;;
;; 另见
;; ----
;; flexvector-fold - 左折叠

;; 基本折叠：收集元素（顺序保持，因为是右折叠）
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-fold-right (lambda (acc x) (cons x acc)) '() fv)
         => '(10 20 30)))

;; 与左折叠对比
(let ((fv (flexvector 1 2 3)))
  ;; 左折叠：((0 - 1) - 2) - 3 = -6
  (check (flexvector-fold - 0 fv) => -6)
  ;; 右折叠：1 - (2 - (3 - 0)) = 2
  (check (flexvector-fold-right - 0 fv) => 2))

;; 构建列表时与左折叠的区别
(let ((fv (flexvector 1 2 3)))
  ;; 左折叠：倒序收集
  (check (flexvector-fold (lambda (acc x) (cons x acc)) '() fv)
         => '(3 2 1))
  ;; 右折叠：正序收集
  (check (flexvector-fold-right (lambda (acc x) (cons x acc)) '() fv)
         => '(1 2 3)))

;; 空向量返回初始值
(check (flexvector-fold-right + 100 (flexvector)) => 100)

;; 单元素
(let ((fv (flexvector 'a)))
  (check (flexvector-fold-right (lambda (acc x) (cons x acc)) '() fv)
         => '(a)))

;; 多向量折叠
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector-fold-right (lambda (acc x y) (+ acc x y)) 0 fv1 fv2)
         => 66))  ; (1+10) + (2+20) + (3+30) = 66

;; 多向量长度不同取最短
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20)))
  (check (flexvector-fold-right (lambda (acc x y) (+ acc (* x y))) 0 fv1 fv2)
         => 50))  ; 1*10 + 2*20 = 50

;; 连接字符串（保持顺序）
(let ((fv (flexvector #\h #\e #\l #\l #\o)))
  (check (flexvector-fold-right (lambda (acc ch) (string-append (string ch) acc))
                                ""
                                fv)
         => "hello"))

(check-report)
