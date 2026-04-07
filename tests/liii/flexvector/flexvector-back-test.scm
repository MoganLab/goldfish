(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-back
;; 返回可变长向量的最后一个元素。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-back fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，必须非空。
;;
;; 返回值
;; -----
;; 返回最后一个元素。
;;
;; 错误
;; ----
;; 对空向量调用会抛出错误。
;;
;; 另见
;; ----
;; flexvector-front - 获取第一个元素

;; 基本测试
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-back fv) => 'c)
) ;let

;; 单元素向量
(let ((fv (flexvector 'only)))
  (check (flexvector-back fv) => 'only)
) ;let

;; 添加元素后更新
(let ((fv (flexvector 'a)))
  (flexvector-add-back! fv 'b)
  (flexvector-add-back! fv 'c)
  (check (flexvector-back fv) => 'c)
) ;let

;; 移除后更新
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-remove-back! fv)
  (check (flexvector-back fv) => 'b)
) ;let

;; 多元素追加后的最后一个
(let ((fv (flexvector 1)))
  (flexvector-add-back! fv 2 3 4)
  (check (flexvector-back fv) => 4)
) ;let

(check-report)
