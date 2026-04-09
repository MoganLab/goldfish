(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-fold
;; 从左到右折叠（左折叠）。时间复杂度 O(n)。
;;
;; 注意：SRFI-214 的 fold 顺序与 SRFI-1 列表 fold 不同，
;; 其累积器在参数第一位: (proc acc x) 而非 (proc x acc)
;;
;; 语法
;; ----
;; (flexvector-fold proc nil fv)
;; (flexvector-fold proc nil fv1 fv2 ...)
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
;; flexvector-fold-right - 右折叠

;; 基本折叠：收集元素（注意顺序是反的，因为是左折叠）
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-fold (lambda (acc x) (cons x acc)) '() fv)
         => '(30 20 10)
  ) ;check
) ;let

;; 求和
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector-fold + 0 fv) => 15)
) ;let

;; 求积
(let ((fv (flexvector 1 2 3 4)))
  (check (flexvector-fold * 1 fv) => 24)
) ;let

;; 空向量返回初始值
(check (flexvector-fold + 100 (flexvector)) => 100)

;; 单元素
(let ((fv (flexvector 'a)))
  (check (flexvector-fold (lambda (acc x) (cons x acc)) '() fv)
         => '(a)
  ) ;check
) ;let

;; 多向量折叠
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector-fold (lambda (acc x y) (+ acc x y)) 0 fv1 fv2)
         => 66  ; (1+10) + (2+20) + (3+30) = 66
  ) ;check
) ;let

;; 多向量长度不同取最短
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20)))
  (check (flexvector-fold (lambda (acc x y) (+ acc (* x y))) 0 fv1 fv2)
         => 50  ; 1*10 + 2*20 = 50
  ) ;check
) ;let

;; 使用字符串累积
(let ((fv (flexvector #\a #\b #\c)))
  (check
    (flexvector-fold (lambda (acc ch) (string-append acc (string ch)))
                     ""
                     fv
    ) ;flexvector-fold
         => "abc"
  ) ;check
) ;let

(check-report)
