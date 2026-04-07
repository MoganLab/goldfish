(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-cumulate
;; 计算累积结果，返回每个位置的累积值组成的新向量。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-cumulate proc nil fv)
;;
;; 参数
;; ----
;; proc : procedure
;;   (proc accumulator element) 返回新的累积值。
;;
;; nil : any
;;   初始累积值。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，第 i 个元素是前 i+1 个元素的累积结果。
;;
;; 示例
;; ----
;; ;; 前缀和
;; (flexvector-cumulate + 0 (flexvector 1 2 3 4))
;; => #(1 3 6 10)
;; ;; 1, 1+2=3, 1+2+3=6, 1+2+3+4=10
;;
;; 另见
;; ----
;; flexvector-fold - 折叠（只返回最终结果）

;; 基本累积：前缀和
(check (flexvector->vector
         (flexvector-cumulate + 0 (flexvector 3 1 4 1 5 9 2 5 6)))
       => #(3 4 8 9 14 23 25 30 36)
) ;check
;; 3, 3+1=4, 4+4=8, 8+1=9, 9+5=14, ...

;; 前缀积
(let ((fv (flexvector 2 3 4 5)))
  (check (flexvector->vector (flexvector-cumulate * 1 fv))
         => #(2 6 24 120)
  ) ;check
) ;let
;; 2, 2*3=6, 6*4=24, 24*5=120

;; 累积字符串
(let ((fv (flexvector #\h #\e #\l #\l #\o)))
  (check (flexvector->vector (flexvector-cumulate (lambda (acc ch)
                                                    (string-append acc (string ch)))
                                                  ""
                                                  fv))
         => #("h" "he" "hel" "hell" "hello")
  ) ;check
) ;let

;; 空向量
(check (flexvector->vector (flexvector-cumulate + 0 (flexvector)))
       => #()
) ;check

;; 单元素
(let ((fv (flexvector 42)))
  (check (flexvector->vector (flexvector-cumulate + 0 fv))
         => #(42)
  ) ;check
) ;let

;; 累积最大值
(let ((fv (flexvector 3 1 4 1 5 9 2 6)))
  (check (flexvector->vector (flexvector-cumulate max 0 fv))
         => #(3 3 4 4 5 9 9 9)
  ) ;check
) ;let

;; 原向量不变
(let ((fv (flexvector 1 2 3)))
  (flexvector-cumulate + 0 fv)
  (check (flexvector->vector fv) => #(1 2 3))
) ;let

(check-report)
