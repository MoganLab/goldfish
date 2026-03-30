(import (liii alist)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; vector->alist
;; 把向量转换成以索引为键的关联列表。
;;
;; 语法
;; ----
;; (vector->alist vector)
;;
;; 参数
;; ----
;; vector : vector?
;; 待转换的向量。
;;
;; 返回值
;; ----
;; list
;; 返回一个关联列表，索引从0开始。
;;
;; 注意
;; ----
;; 空向量会转换成空列表。
;;
;; 示例
;; ----
;; (vector->alist #()) => '()
;; (vector->alist #(42)) => '((0 . 42))
;;
;; 错误处理
;; ----
;; type-error 当参数不是向量时抛出。

(check (vector->alist #()) => '())
(check (vector->alist #(42)) => '((0 . 42)))
(check (vector->alist #("a" "b" "c")) => '((0 . "a") (1 . "b") (2 . "c")))
(check (vector->alist #(#(1 2) #(3 4))) => '((0 . #(1 2)) (1 . #(3 4))))
(check-catch 'type-error (vector->alist 'not-a-vector))

(check-report)
