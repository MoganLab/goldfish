(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector
;; 创建包含指定元素的向量。
;;
;; 语法
;; ----
;; (vector obj ...)
;;
;; 参数
;; ----
;; obj : any?
;; 要放入向量的元素，可以是任意类型。
;;
;; 返回值
;; ----
;; vector
;; 一个新的向量，元素顺序与参数顺序一致。
;;
;; 注意
;; ----
;; 当不传任何参数时，返回空向量。
;;
;; 示例
;; ----
;; (vector 1 2 3) => #(1 2 3)
;; (vector) => #()
;;
;; 错误处理
;; ----
;; 无


(check (vector) => #())
(check (vector 1 2 3) => #(1 2 3))
(check (vector 'a 'b 'c) => #(a b c))
(check (vector 1 2.5 "hello" 'symbol #\c #t #f)
  =>
  #(1 2.5 "hello" symbol #\c #t #f)
) ;check
(check (vector 42) => #(42))


(check-report)
