(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-fold-right
;; 从右到左折叠（右折叠）。时间复杂度 O(n)。
;;
;; 注意：fold-right 的参数顺序与 fold 不同
;; fold: (proc acc element) - 累积器在前
;; fold-right: (proc element acc) - 元素在前，累积器在后
;;
;; 语法
;; ----
;; (flexvector-fold-right proc nil fv)
;; (flexvector-fold-right proc nil fv1 fv2 ...)
;;
;; 参数
;; ----
;; proc : procedure
;;   (proc element accumulator) 或 (proc e1 e2 ... accumulator)
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
  (check (flexvector-fold-right cons '() fv)
         => '(10 20 30)
  ) ;check
) ;let

;; 与左折叠对比
(let ((fv (flexvector 1 2 3)))
  ;; 左折叠：((0 - 1) - 2) - 3 = -6
  (check (flexvector-fold - 0 fv) => -6)
  ;; 右折叠：1 - (2 - (3 - 0)) = 2
  (check (flexvector-fold-right - 0 fv) => 2)
) ;let

;; 构建列表时与左折叠的区别
(let ((fv (flexvector 1 2 3)))
  ;; 左折叠：倒序收集
  (check (flexvector-fold (lambda (acc x) (cons x acc)) '() fv)
         => '(3 2 1)
  ) ;check
  ;; 右折叠：正序收集 (参数顺序为 element acc)
  (check (flexvector-fold-right cons '() fv)
         => '(1 2 3)
  ) ;check
) ;let

;; 空向量返回初始值
(check (flexvector-fold-right + 100 (flexvector)) => 100)

;; 单元素
(let ((fv (flexvector 'a)))
  (check (flexvector-fold-right cons '() fv)
         => '(a)
  ) ;check
) ;let

;; 多向量折叠 (参数顺序为 element1 element2 ... acc)
(let ((fv1 (flexvector 1 2 3))
      (fv2 (flexvector 10 20 30)))
  (check (flexvector-fold-right (lambda (x y acc) (+ x y acc)) 0 fv1 fv2)
         => 66  ; (1+10) + (2+20) + (3+30) = 66
  ) ;check
) ;let

;; 多向量长度不同取最短 (参数顺序为 element1 element2 acc)
(let ((fv1 (flexvector 1 2 3 4))
      (fv2 (flexvector 10 20)))
  (check (flexvector-fold-right (lambda (x y acc) (+ (* x y) acc)) 0 fv1 fv2)
         => 50  ; 1*10 + 2*20 = 50
  ) ;check
) ;let

;; 连接字符串（保持顺序）(参数顺序为 element acc)
(let ((fv (flexvector #\h #\e #\l #\l #\o)))
  (check (flexvector-fold-right (lambda (ch acc) (string-append (string ch) acc))
                                ""
                                fv)
         => "hello"
  ) ;check
) ;let

(check-report)
