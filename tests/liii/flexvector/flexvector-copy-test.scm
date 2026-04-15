(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-copy
;; 创建 flexvector 的浅拷贝。时间复杂度 O(n)，n 为拷贝长度。
;;
;; 语法
;; ----
;; (flexvector-copy fv)
;; (flexvector-copy fv start)
;; (flexvector-copy fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;;   源向量。
;;
;; start : exact-nonnegative-integer (可选，默认 0)
;;   起始索引（包含）。
;;
;; end : exact-nonnegative-integer (可选，默认长度)
;;   结束索引（不包含）。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，是原向量的浅拷贝。
;;
;; 示例
;; ----
;; (define fv (flexvector 1 2 3))
;; (define cp (flexvector-copy fv))
;; ;; cp 是独立的对象
;; (eq? fv cp)                                => #f
;; ;; 修改 cp 不影响 fv
;; (flexvector-set! cp 0 'x)
;; (flexvector-ref fv 0)                      => 1


;; 完整拷贝
(let ((fv (flexvector 1 2 3)))
  (let ((copy (flexvector-copy fv)))
    ;; 长度相同
    (check (flexvector-length fv)
      =>
      (flexvector-length copy)
    ) ;check
    ;; 是不同对象
    (check-false (eq? fv copy))
    ;; 内容相同
    (check (flexvector->vector copy)
      =>
      #(1 2 3)
    ) ;check
    ;; 修改拷贝不影响原向量
    (flexvector-set! copy 0 'x)
    (check (flexvector-ref fv 0) => 1)
    (check (flexvector-ref copy 0) => 'x)
  ) ;let
) ;let


;; 从指定位置拷贝到末尾
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector->vector (flexvector-copy fv 2)
         ) ;flexvector->vector
    =>
    #(3 4 5)
  ) ;check
) ;let


;; 拷贝区间 [start, end)
(let ((fv (flexvector 1 2 3 4 5)))
  (check (flexvector->vector (flexvector-copy fv 1 4)
         ) ;flexvector->vector
    =>
    #(2 3 4)
  ) ;check
) ;let


;; 边界测试：空区间
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector (flexvector-copy fv 0 0)
         ) ;flexvector->vector
    =>
    #()
  ) ;check
  (check (flexvector->vector (flexvector-copy fv 3 3)
         ) ;flexvector->vector
    =>
    #()
  ) ;check
) ;let


;; 单元素区间
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector (flexvector-copy fv 1 2)
         ) ;flexvector->vector
    =>
    #(2)
  ) ;check
) ;let


(check-report)
