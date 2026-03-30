(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-map->list
;; 映射并转换为列表。
;;
;; 语法
;; ----
;; (fxmapping-map->list proc fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数，接收 key 和 value，返回列表元素。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回 proc 应用于所有键值对的结果列表（按键降序）。
;;
(check (fxmapping-map->list (lambda (k v) (cons k v)) (fxmapping 0 'a 1 'b)) => '((0 . a) (1 . b)))

(check-report)
