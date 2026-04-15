(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; last 函数测试
;;
;; 语法
;; ----
;; (last lst)
;;
;; 参数
;; ----
;; lst : pair?
;; 非空列表或点对结构。
;;
;; 返回值
;; ------
;; any
;; 返回列表的最后一个元素。
;;
;; 示例
;; ----
;; (last '(a b c)) => 'c
;; (last '(c)) => 'c
;; (last '(a b . c)) => 'b
;; (last '(b . c)) => 'b


(check (last '(a b c)) => 'c)
(check (last '(c)) => 'c)


(check (last '(a b . c)) => 'b)
(check (last '(b . c)) => 'b)


(check-catch 'wrong-type-arg (last '()))


(check-report)
