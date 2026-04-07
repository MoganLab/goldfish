(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-add-back!
;; 向可变长向量尾部追加元素。平均时间复杂度 O(1)，扩容时 O(n)。
;;
;; 语法
;; ----
;; (flexvector-add-back! fv element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; element ... : any
;;   要添加的一个或多个元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector（即输入的 fv）。
;;
;; 副作用
;; -----
;; 修改 fv，在尾部添加元素。必要时自动扩容。
;;
;; 示例
;; ----
;; ;; 单个元素
;; (define fv (flexvector 'a 'b))
;; (flexvector-add-back! fv 'c)
;; (flexvector->list fv)              => (a b c)
;;
;; ;; 多个元素
;; (define fv2 (flexvector 1))
;; (flexvector-add-back! fv2 2 3 4)
;; (flexvector->list fv2)             => (1 2 3 4)
;;
;; ;; 从空向量构建
;; (define fv3 (flexvector))
;; (flexvector-add-back! fv3 'x)
;; (flexvector-add-back! fv3 'y)
;; (flexvector->list fv3)             => (x y)

;; 基本测试
(let ((fv (flexvector)))
  (flexvector-add-back! fv 'a)
  (check (flexvector-length fv) => 1)
  (check (flexvector-ref fv 0) => 'a)
) ;let

;; 追加到已有元素后
(let ((fv (flexvector 'x 'y 'z)))
  (flexvector-add-back! fv 'w)
  (check (flexvector-length fv) => 4)
  (check (flexvector-ref fv 3) => 'w)
  (check (flexvector->list fv) => '(x y z w))
) ;let

;; 追加多个元素
(let ((fv (flexvector 1)))
  (flexvector-add-back! fv 2 3 4)
  (check (flexvector-length fv) => 4)
  (check (flexvector->vector fv) => #(1 2 3 4))
) ;let

;; 返回值是原对象
(let ((fv (flexvector 1 2)))
  (check (eq? (flexvector-add-back! fv 3) fv) => #t)
) ;let

;; 测试扩容（初始容量为4，添加超过4个元素触发扩容）
(let ((fv (flexvector)))
  (do ((i 0 (+ i 1)))
      ((= i 10))
    (flexvector-add-back! fv i)
  ) ;do
  (check (flexvector-length fv) => 10)
  (check (flexvector-ref fv 9) => 9)
) ;let

(check-report)
