(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-add-front!
;; 向可变长向量前端（头部）插入元素。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-add-front! fv element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; element ... : any
;;   要插入的一个或多个元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector（即输入的 fv）。
;;
;; 副作用
;; -----
;; 修改 fv，在前端插入元素。
;;
;; 注意：比 flexvector-add-back! 慢，因为需要移动现有元素。
;;
;; 另见
;; ----
;; flexvector-add-back! - 尾部添加（更快）
;; flexvector-add! - 指定位置添加

;; 单个元素
(let ((fv (flexvector 'a)))
  (flexvector-add-front! fv 'b)
  (check (flexvector-ref fv 0) => 'b)
  (check (flexvector-ref fv 1) => 'a)
  (check (flexvector->list fv) => '(b a)))

;; 多个元素
(let ((fv (flexvector 'x)))
  (flexvector-add-front! fv 'a 'b 'c)
  (check (flexvector->list fv) => '(a b c x)))

;; 添加到空向量
(let ((fv (flexvector)))
  (flexvector-add-front! fv 'first)
  (check (flexvector->list fv) => '(first)))

;; 返回值是原对象
(let ((fv (flexvector 1 2)))
  (check (eq? (flexvector-add-front! fv 0) fv) => #t))

;; 长度变化
(let ((fv (flexvector 1 2 3)))
  (check (flexvector-length fv) => 3)
  (flexvector-add-front! fv 0)
  (check (flexvector-length fv) => 4))

;; 多次添加
(let ((fv (flexvector)))
  (flexvector-add-front! fv 'c)
  (flexvector-add-front! fv 'b)
  (flexvector-add-front! fv 'a)
  (check (flexvector->list fv) => '(a b c)))

(check-report)
