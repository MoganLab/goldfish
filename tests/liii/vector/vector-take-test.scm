(import (liii check)
  (liii error)
  (liii vector)
) ;import


(check-set-mode! 'report-failed)


;; vector-take
;; 从左侧取出指定数量元素，对越界情况容忍。
;;
;; 语法
;; ----
;; (vector-take vec n)
;;
;; 参数
;; ----
;; vec : vector?
;; 源向量。
;;
;; n : integer?
;; 要提取的元素数量。
;;
;; 返回值
;; ----
;; vector
;; 一个包含前n个元素的新向量。
;;
;; 注意
;; ----
;; 当n小于0时返回空向量；当n大于向量长度时返回整个向量。
;;
;; 示例
;; ----
;; (vector-take #(1 2 3 4 5) 3) => #(1 2 3)
;; (vector-take #(1 2 3) 10) => #(1 2 3)
;;
;; 错误处理
;; ----
;; type-error 当vec不是向量，或n不是整数时


(check (vector-take #(1 2 3 4 5) 3)
  =>
  #(1 2 3)
) ;check
(check (vector-take #(1 2 3 4 5) 0)
  =>
  #()
) ;check
(check (vector-take #(1 2 3 4 5) 5)
  =>
  #(1 2 3 4 5)
) ;check
(check (vector-take #(1 2 3) -1) => #())
(check (vector-take #(1 2 3) 10)
  =>
  #(1 2 3)
) ;check
(check (vector-take #() 0) => #())
(check (vector-take #() 5) => #())
(check-catch 'type-error
  (vector-take "not a vector" 2)
) ;check-catch
(check-catch 'type-error
  (vector-take #(1 2 3) "not a number")
) ;check-catch


(check-report)
