(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; flat-map 函数测试
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
;;
;; 说明
;; ----
;; flat-map函数类似于append-map，将映射结果展平。
;;
;; 示例
;; ----
;; (flat-map (lambda (x) (list x x)) (list 1 2 3)) => (list 1 1 2 2 3 3)


(check (flat-map (lambda (x) (list x x))
         (list 1 2 3)
       ) ;flat-map
  =>
  (list 1 1 2 2 3 3)
) ;check


(check-catch 'type-error
  (flat-map 1 (list 1 2 3))
) ;check-catch


(check-report)
