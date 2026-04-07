(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-empty?
;; 检查可变长向量是否为空（长度为0）。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-empty? fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   要检查的向量。
;;
;; 返回值
;; -----
;; 如果向量为空返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 非 flexvector 参数会抛出错误。
;;
;; 另见
;; ----
;; flexvector-length - 获取长度
;; flexvector? - 检查类型

;; 空向量
(check-true (flexvector-empty? (flexvector)))
(check-true (flexvector-empty? (make-flexvector 0)))
(check-true (flexvector-empty? (list->flexvector '())))

;; 非空向量
(check-false (flexvector-empty? (flexvector 1 2 3)))
(check-false (flexvector-empty? (make-flexvector 1)))
(check-false (flexvector-empty? (list->flexvector '(a))))

;; 添加后变为非空
(let ((fv (flexvector)))
  (check-true (flexvector-empty? fv))
  (flexvector-add-back! fv 'x)
  (check-false (flexvector-empty? fv))
) ;let

;; 删除后变为空
(let ((fv (flexvector 'a)))
  (check-false (flexvector-empty? fv))
  (flexvector-remove-back! fv)
  (check-true (flexvector-empty? fv))
) ;let

(check-report)
