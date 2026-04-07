(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-skip
;; 跳过满足条件的前缀元素，返回第一个不满足条件的索引。时间复杂度 O(n)。
;;
;; 等价于 (flexvector-index (complement pred?) fv)
;;
;; 语法
;; ----
;; (flexvector-skip pred? fv)
;; (flexvector-skip pred? fv ...)
;;
;; 参数
;; ----
;; pred? : procedure
;;   谓词函数。
;;
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回第一个不满足 pred? 的元素的索引。
;; 如果全部满足，返回向量长度。
;; 如果开头就不满足，返回 0。
;; 空向量返回 0。
;;
;; 另见
;; ----
;; flexvector-skip-right - 从右侧跳过
;; flexvector-index - 查找满足条件的索引

;; 基本测试：跳过小于25的元素
(let ((fv (flexvector 10 20 30)))
  ;; 10<25 满足, 20<25 满足, 30<25 不满足
  ;; 所以返回索引 2
  (check (flexvector-skip (lambda (x) (< x 25)) fv) => 2)
) ;let

;; 从开头跳过偶数
(let ((fv (flexvector 2 4 6 7 8)))
  ;; 2,4,6 是偶数，7 不是
  (check (flexvector-skip even? fv) => 3)
) ;let

;; 第一个就不满足
(let ((fv (flexvector 1 2 3)))
  (check (flexvector-skip even? fv) => 0)
) ;let

;; 全部满足
(let ((fv (flexvector 2 4 6)))
  (check (flexvector-skip even? fv) => 3)  ; 返回长度
) ;let

;; 空向量
(check (flexvector-skip (lambda (x) #t) (flexvector)) => 0)

;; 单元素满足
(let ((fv (flexvector 42)))
  (check (flexvector-skip (lambda (x) (= x 42)) fv) => 1)
) ;let

;; 单元素不满足
(let ((fv (flexvector 42)))
  (check (flexvector-skip (lambda (x) (= x 0)) fv) => 0)
) ;let

(check-report)
