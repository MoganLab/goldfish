(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-remove!
;; 从 flexvector 中移除指定位置的元素，返回被移除的元素。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-remove! fv index)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; index : exact-nonnegative-integer
;;   要移除元素的索引。
;;
;; 返回值
;; -----
;; 返回被移除的元素。
;;
;; 副作用
;; -----
;; 修改 fv，移除指定位置的元素，后续元素前移。
;;
;; 另见
;; ----
;; flexvector-remove-front! - 移除头部
;; flexvector-remove-back! - 移除尾部
;; flexvector-remove-range! - 移除区间

;; 基本测试：移除中间元素
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove! fv 1) => 'b)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 0) => 'a)
  (check (flexvector-ref fv 1) => 'c)
) ;let

;; 移除第一个元素
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove! fv 0) => 'a)
  (check (flexvector->list fv) => '(b c))
) ;let

;; 移除最后一个元素
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove! fv 2) => 'c)
  (check (flexvector->list fv) => '(a b))
) ;let

;; 单元素向量
(let ((fv (flexvector 'only)))
  (check (flexvector-remove! fv 0) => 'only)
  (check (flexvector-empty? fv) => #t)
) ;let

;; 移除后继续操作
(let ((fv (flexvector 'a 'b 'c 'd 'e)))
  (flexvector-remove! fv 2)  ; 移除 'c
  (flexvector-add-back! fv 'x)
  (check (flexvector->list fv) => '(a b d e x))
) ;let

(check-report)
