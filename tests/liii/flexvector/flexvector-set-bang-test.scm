(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-set!
;; 设置指定位置的元素，返回原值。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector-set! fv index value)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; index : exact-nonnegative-integer
;;   元素索引，从 0 开始。
;;
;; value : any
;;   新值。
;;
;; 返回值
;; -----
;; 返回该位置原来的值。
;;
;; 副作用
;; -----
;; 修改 fv 指定位置的元素。
;;
;; 错误
;; ----
;; 索引越界会抛出错误。
;;
;; 示例
;; ----
;; (define fv (flexvector 'a 'b 'c))
;; (flexvector-set! fv 1 'd)     => 'b    ; 返回原值
;; (flexvector->list fv)         => (a d c)  ; 已修改
;;
;; ;; 设置首尾元素
;; (flexvector-set! fv 0 'x)     => 'a
;; (flexvector-set! fv 2 'z)     => 'c

(let ((fv (flexvector 'a 'b 'c)))
  ;; 返回原值
  (check (flexvector-set! fv 1 'd) => 'b)
  ;; 验证已修改
  (check (flexvector-ref fv 1) => 'd)
  ;; 验证其他元素未变
  (check (flexvector->list fv) => '(a d c))
) ;let

;; 边界测试：首尾元素
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-set! fv 0 'x) => 'a)
  (check (flexvector-set! fv 2 'z) => 'c)
  (check (flexvector->list fv) => '(x b z))
) ;let

;; 单元素向量
(let ((fv (flexvector 'only)))
  (check (flexvector-set! fv 0 'new) => 'only)
  (check (flexvector-ref fv 0) => 'new)
) ;let

;; 多次修改
(let ((fv (make-flexvector 3 0)))
  (flexvector-set! fv 0 10)
  (flexvector-set! fv 1 20)
  (flexvector-set! fv 2 30)
  (check (flexvector->vector fv) => #(10 20 30))
) ;let

(check-report)
