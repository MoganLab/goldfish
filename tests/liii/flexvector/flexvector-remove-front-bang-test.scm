(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-remove-front!
;; 从可变长向量前端（头部）移除一个元素，返回被移除的元素。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-remove-front! fv)
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
;; 修改 fv，移除第一个元素。剩余元素前移。
;;
;; 注意：比 flexvector-remove-back! 慢，因为需要移动剩余元素。
;;
;; 另见
;; ----
;; flexvector-remove-back! - 尾部移除（更快）
;; flexvector-remove! - 指定位置移除

;; 基本测试
(let ((fv (flexvector 'a 'b 'c)))
  ;; 返回被移除的元素
  (check (flexvector-remove-front! fv) => 'a)
  ;; 长度减少
  (check (flexvector-length fv) => 2)
  ;; 剩余元素前移
  (check (flexvector-ref fv 0) => 'b)
  (check (flexvector->list fv) => '(b c))
) ;let

;; 继续移除直到空
(let ((fv (flexvector 'x 'y)))
  (flexvector-remove-front! fv)
  (flexvector-remove-front! fv)
  (check (flexvector-empty? fv) => #t)
) ;let

;; 单元素向量
(let ((fv (flexvector 'only)))
  (check (flexvector-remove-front! fv) => 'only)
  (check (flexvector-empty? fv) => #t)
) ;let

;; 添加后再移除（使用多个元素一次性添加）
(let ((fv (flexvector)))
  (flexvector-add-front! fv 1 2 3)
  (check (flexvector->list fv) => '(1 2 3))
  (check (flexvector-remove-front! fv) => 1)
  (check (flexvector->list fv) => '(2 3))
) ;let

;; 移除后添加
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-remove-front! fv)
  (flexvector-add-back! fv 'd)
  (check (flexvector->list fv) => '(b c d))
) ;let

(check-report)
