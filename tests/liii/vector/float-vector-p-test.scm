(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; float-vector?
;; 判断对象是否为浮点数向量。
;;
;; 语法
;; ----
;; (float-vector? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果obj是浮点数向量则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 普通向量即使元素全为实数，也不一定被视为float-vector。
;;
;; 示例
;; ----
;; (float-vector? (float-vector 1.0 2.0 3.0)) => #t
;; (float-vector? (vector 1.0 2.0 3.0)) => #f
;;
;; 错误处理
;; ----
;; 无

(check-true (float-vector? (float-vector 1.0 2.0 3.0)))
(check-false (float-vector? (vector 1.0 2.0 3.0)))
(check-false (float-vector? (int-vector 1 2 3)))
(check-false (float-vector? (complex-vector 1+2i 3+4i)))
(check-false (float-vector? 'not-a-vector))
(check-false (float-vector? 42))
(check-false (float-vector? 3.14))
(check-false (float-vector? "string"))

(check-report)
