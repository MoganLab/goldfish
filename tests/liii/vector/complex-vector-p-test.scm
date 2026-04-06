(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; complex-vector?
;; 判断对象是否为复数向量。
;;
;; 语法
;; ----
;; (complex-vector? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果obj是复数向量则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 普通向量即使元素全为复数，也不一定被视为complex-vector。
;;
;; 示例
;; ----
;; (complex-vector? (complex-vector 1+2i 3+4i)) => #t
;; (complex-vector? (vector 1+2i 3+4i)) => #f
;;
;; 错误处理
;; ----
;; 无

(check-true (complex-vector? (complex-vector 1+2i 3+4i)))
(check-false (complex-vector? (vector 1+2i 3+4i)))
(check-false (complex-vector? (int-vector 1 2 3)))
(check-false (complex-vector? 'not-a-vector))
(check-false (complex-vector? 42))
(check-false (complex-vector? 1+2i))
(check-false (complex-vector? "string"))

(check-report)
