(import (liii check)
        (liii error)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-drop
;; 从左侧丢弃指定数量元素，对越界情况容忍。
;;
;; 语法
;; ----
;; (vector-drop vec n)
;;
;; 参数
;; ----
;; vec : vector?
;; 源向量。
;;
;; n : integer?
;; 要丢弃的元素数量。
;;
;; 返回值
;; ----
;; vector
;; 一个移除前n个元素后的新向量。
;;
;; 注意
;; ----
;; 当n小于0时返回原向量内容；当n大于向量长度时返回空向量。
;;
;; 示例
;; ----
;; (vector-drop #(1 2 3 4 5) 3) => #(4 5)
;; (vector-drop #(1 2 3) 10) => #()
;;
;; 错误处理
;; ----
;; type-error 当vec不是向量，或n不是整数时

(check (vector-drop #(1 2 3 4 5) 3) => #(4 5))
(check (vector-drop #(1 2 3 4 5) 0) => #(1 2 3 4 5))
(check (vector-drop #(1 2 3 4 5) 5) => #())
(check (vector-drop #(1 2 3) -1) => #(1 2 3))
(check (vector-drop #(1 2 3) 10) => #())
(check (vector-drop #() 0) => #())
(check (vector-drop #() 5) => #())
(check-catch 'type-error (vector-drop "not a vector" 2))
(check-catch 'type-error (vector-drop #(1 2 3) "not a number"))

(check-report)
