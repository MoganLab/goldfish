(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; vector-map
;; 对向量中的每个元素应用过程，返回结果向量。
;;
;; 语法
;; ----
;; (vector-map proc vec1 vec2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受元素作为参数的过程。
;;
;; vec1, vec2, ... : vector?
;; 输入向量。
;;
;; 返回值
;; ------
;; vector?
;; 返回包含结果的向量。

;; 基本测试
(check (vector-map (lambda (x) (* x 2)) #(1 2 3)) => #(2 4 6))
(check (vector-map (lambda (x) (+ x 1)) #(0 1 2)) => #(1 2 3))

;; 转换测试
(check (vector-map number->string #(1 2 3)) => #("1" "2" "3"))

;; 空向量测试
(check (vector-map (lambda (x) x) #()) => #())

;; 单元素测试
(check (vector-map (lambda (x) (* x x)) #(5)) => #(25))

;; 恒等测试
(check (vector-map (lambda (x) x) #(a b c)) => #(a b c))

;; 多个向量参数（如果支持）
;(check (vector-map + #(1 2 3) #(4 5 6)) => #(5 7 9))

;; 布尔测试
(check (vector-map not #(#t #f #t)) => #(#f #t #f))

(check-report)
