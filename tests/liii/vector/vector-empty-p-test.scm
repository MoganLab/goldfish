(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-empty?
;; 检查向量是否为空。
;;
;; 语法
;; ----
;; (vector-empty? vec)
;;
;; 参数
;; ----
;; vec : vector?
;; 要检查的向量。
;;
;; 返回值
;; ----
;; boolean
;; 当向量长度为0时返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 这是常量时间的空判断操作。
;;
;; 示例
;; ----
;; (vector-empty? #()) => #t
;; (vector-empty? #(1)) => #f
;;
;; 错误处理
;; ----
;; type-error 当vec不是向量时


(check-true (vector-empty? (vector)))
(check-false (vector-empty? (vector 1)))
(check-false (vector-empty? #(a b c)))
(check-false (vector-empty? #(1 2.5 "hello" 'symbol #\c #t #f)
             ) ;vector-empty?
) ;check-false
(check-catch 'type-error
  (vector-empty? 1)
) ;check-catch


(check-report)
