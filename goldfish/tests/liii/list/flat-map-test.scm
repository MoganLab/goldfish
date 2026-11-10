;; flat-map
;; 将所有映射结果连接并展平成一个列表。
;;
;; 语法
;; ----
;; (flat-map f list)
;;
;; 参数
;; ----
;; f : procedure?
;; 映射函数，返回列表。
;;
;; list : list?
;; 要映射的列表。
;;
;; 返回值
;; ------
;; list
;; 将所有映射结果连接并展平成一个列表。

(import (liii list) (liii check))

(check-set-mode! 'report-failed)

(check (flat-map (lambda (x) (list x x)) (list 1 2 3)) => (list 1 1 2 2 3 3))

(check-catch 'type-error (flat-map 1 (list 1 2 3)))

(check-report)
