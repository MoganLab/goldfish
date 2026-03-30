(import (liii check)
        (liii error)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-drop-right
;; 从右侧丢弃指定数量元素，对越界情况容忍。
;;
;; 语法
;; ----
;; (vector-drop-right vec n)
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
;; 一个移除末尾n个元素后的新向量。
;;
;; 注意
;; ----
;; 当n小于0时返回原向量内容；当n大于向量长度时返回空向量。
;;
;; 示例
;; ----
;; (vector-drop-right #(1 2 3 4 5) 3) => #(1 2)
;;
;; 错误处理
;; ----
;; type-error 当vec不是向量，或n不是整数时

(check (vector-drop-right #(1 2 3 4 5) 3) => #(1 2))
(check (vector-drop-right #(1 2 3 4 5) 0) => #(1 2 3 4 5))
(check (vector-drop-right #(1 2 3 4 5) 5) => #())
(check (vector-drop-right #(1 2 3) -1) => #(1 2 3))
(check (vector-drop-right #(1 2 3) 10) => #())
(check (vector-drop-right #() 0) => #())
(check-catch 'type-error (vector-drop-right "not a vector" 2))
(check-catch 'type-error (vector-drop-right #(1 2 3) "not a number"))

(check-report)
