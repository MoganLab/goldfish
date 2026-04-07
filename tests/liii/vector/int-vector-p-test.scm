(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; int-vector?
;; 判断对象是否为整数向量。
;;
;; 语法
;; ----
;; (int-vector? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果obj是整数向量则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 普通向量即使元素全为整数，也不一定被视为int-vector。
;;
;; 示例
;; ----
;; (int-vector? (int-vector 1 2 3)) => #t
;; (int-vector? (vector 1 2 3)) => #f
;;
;; 错误处理
;; ----
;; 无

(check-true (int-vector? (int-vector 1 2 3)))
(check-false (int-vector? (vector 1 2 3)))

(check-report)
