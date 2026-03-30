(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-every?
;; 检查是否所有键值对都满足谓词。
;;
;; 语法
;; ----
;; (fxmapping-every? pred fxmap)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 如果所有键值对都满足 pred，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-every? (lambda (k v) (> k 0)) (fxmapping 1 'a 2 'b 3 'c)))
(check-false (fxmapping-every? (lambda (k v) (> k 0)) (fxmapping 0 'a 1 'b)))

(check-report)
