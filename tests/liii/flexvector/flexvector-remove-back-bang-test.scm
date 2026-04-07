(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-remove-back!
;; 从可变长向量尾部移除一个元素，返回被移除的元素。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-remove-back! fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。必须非空。
;;
;; 返回值
;; -----
;; 返回被移除的元素。
;;
;; 副作用
;; -----
;; 修改 fv，移除最后一个元素。
;;
;; 错误
;; ----
;; 对空向量调用会抛出错误。
;;
(let ((fv (flexvector 'a 'b 'c)))
  ;; 返回被移除的元素
  (check (flexvector-remove-back! fv) => 'c)
  (check (flexvector-length fv) => 2)
  (check (flexvector->list fv) => '(a b))
  ;; 继续移除
  (check (flexvector-remove-back! fv) => 'b)
  (check (flexvector-remove-back! fv) => 'a)
  ;; 现在为空
  (check (flexvector-empty? fv) => #t)
  (check (flexvector-length fv) => 0)
) ;let

;; 单元素向量
(let ((fv (flexvector 'only)))
  (check (flexvector-remove-back! fv) => 'only)
  (check (flexvector-empty? fv) => #t)
) ;let

;; 添加后再移除
(let ((fv (flexvector 1 2)))
  (flexvector-add-back! fv 3)
  (check (flexvector-remove-back! fv) => 3)
  (check (flexvector->list fv) => '(1 2))
) ;let

(check-report)
