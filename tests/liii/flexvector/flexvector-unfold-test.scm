(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-unfold
;; 通过 unfold 构造 flexvector。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-unfold pred? gen succ seed)
;; (flexvector-unfold pred? gen succ seed1 seed2 ...)
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
;; 返回新的 flexvector。
;;
;; 另见
;; ----
;; flexvector-unfold-right - 从右展开
;; generator->flexvector - 从生成器构造

;; 基本展开：生成平方数直到超过100
(check (flexvector->vector
         (flexvector-unfold (lambda (x) (> x 10))
                            (lambda (x) (* x x))
                            (lambda (x) (+ x 1))
                            1)
         ) ;flexvector-unfold
       => #(1 4 9 16 25 36 49 64 81 100)
) ;check

;; 生成列表
(let
  ((result
     (flexvector->list
       (flexvector-unfold (lambda (n) (< n 0))
                          (lambda (n) n)
                          (lambda (n) (- n 1))
                          5)
       ) ;flexvector-unfold
     ) ;flexvector->list
   ) ;result
  (check result => '(5 4 3 2 1 0))
) ;let

;; 斐波那契数列
(let
  ((result
     (flexvector->list
       (flexvector-unfold (lambda (p) (> (car p) 100))
                          (lambda (p) (car p))
                          (lambda (p) (list (cadr p) (+ (car p) (cadr p))))
                          '(1 1))
       ) ;flexvector-unfold
     ) ;flexvector->list
   ) ;result
  (check result => '(1 1 2 3 5 8 13 21 34 55 89))
) ;let

;; 空结果
(check (flexvector->vector
         (flexvector-unfold (lambda (x) #t)
                            (lambda (x) x)
                            (lambda (x) x)
                            'seed)
         ) ;flexvector-unfold
       => #()
) ;check

;; 单元素
(check (flexvector->vector
         (flexvector-unfold (lambda (x) (> x 0))
                            (lambda (x) x)
                            (lambda (x) (+ x 1))
                            0)
         ) ;flexvector-unfold
       => #(0)
) ;check

;; 使用字符串状态
(check (flexvector->vector
         (flexvector-unfold (lambda (s) (string=? s ""))
                            (lambda (s) (string-ref s 0))
                            (lambda (s) (substring s 1 (string-length s)))
                            "hello")
         ) ;flexvector-unfold
       => #(#\h #\e #\l #\l #\o)
) ;check

(check-report)
