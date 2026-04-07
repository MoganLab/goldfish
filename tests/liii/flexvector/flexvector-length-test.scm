(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-length
;; 返回可变长向量的元素个数。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-length fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量。
;;
;; 返回值
;; -----
;; exact-nonnegative-integer - 向量中元素的数量。
;;
;; 示例
;; ----
;; ;; 空向量
;; (flexvector-length (flexvector))           => 0
;;
;; ;; 多元素向量
;; (flexvector-length (flexvector 1 2 3))     => 3
;;
;; ;; 修改后长度变化
;; (define fv (flexvector 'a 'b))
;; (flexvector-length fv)                     => 2
;; (flexvector-add-back! fv 'c)
;; (flexvector-length fv)                     => 3

;; 基本测试
(check (flexvector-length (flexvector)) => 0)
(check (flexvector-length (flexvector 1)) => 1)
(check (flexvector-length (flexvector 1 2 3)) => 3)

;; 验证修改后长度变化
(let ((fv (flexvector 'a 'b)))
  (check (flexvector-length fv) => 2)
  (flexvector-add-back! fv 'c)
  (check (flexvector-length fv) => 3)
  (flexvector-remove-back! fv)
  (check (flexvector-length fv) => 2)
) ;let

;; 大量元素测试（测试扩容机制）
(let ((fv (flexvector)))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (flexvector-add-back! fv i)
  ) ;do
  (check (flexvector-length fv) => 100)
) ;let

(check-report)
