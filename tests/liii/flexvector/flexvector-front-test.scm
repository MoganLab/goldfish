(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-front
;; 返回可变长向量的第一个元素。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-front fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，必须非空。
;;
;; 返回值
;; -----
;; 返回第一个元素。
;;
;; 错误
;; ----
;; 对空向量调用会抛出错误。
;;
;; 另见
;; ----
;; flexvector-back - 获取最后一个元素
;; flexvector-ref - 按索引访问任意元素

;; 基本测试
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-front fv) => 'a))

;; 单元素向量
(let ((fv (flexvector 'only)))
  (check (flexvector-front fv) => 'only))

;; 添加元素后
(let ((fv (flexvector)))
  (flexvector-add-back! fv 'first)
  (flexvector-add-back! fv 'second)
  (check (flexvector-front fv) => 'first))

;; 移除后更新
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-remove-front! fv)
  (check (flexvector-front fv) => 'b))

(check-report)
