(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-clear!
;; 清空可变长向量，移除所有元素。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-clear! fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector（即输入的 fv）。
;;
;; 副作用
;; -----
;; 移除 fv 中所有元素，长度变为 0。
;;
;; 另见
;; ----
;; flexvector-remove-range! - 移除指定范围
;; flexvector-empty? - 检查是否为空

;; 基本测试
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-clear! fv)
  (check (flexvector-length fv) => 0)
  (check (flexvector-empty? fv) => #t))

;; 清空后可以继续使用
(let ((fv (flexvector 1 2 3)))
  (flexvector-clear! fv)
  (flexvector-add-back! fv 'x)
  (flexvector-add-back! fv 'y)
  (check (flexvector->list fv) => '(x y)))

;; 空向量清空（无副作用）
(let ((fv (flexvector)))
  (flexvector-clear! fv)
  (check (flexvector-empty? fv) => #t))

;; 单元素清空
(let ((fv (flexvector 'only)))
  (flexvector-clear! fv)
  (check (flexvector-empty? fv) => #t))

;; 返回值是原对象
(let ((fv (flexvector 1 2 3)))
  (check (eq? (flexvector-clear! fv) fv) => #t))

;; 清空后设置
(let ((fv (flexvector 'a 'b)))
  (flexvector-clear! fv)
  (flexvector-add-back! fv 1)
  (check (flexvector-ref fv 0) => 1))

(check-report)
