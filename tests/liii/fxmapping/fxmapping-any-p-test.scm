(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-any?
;; 检查是否存在满足谓词的键值对。
;;
;; 语法
;; ----
;; (fxmapping-any? pred fxmap)
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
;; 如果存在满足 pred 的键值对，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-any? (lambda (k v) (> k 5))
              (fxmapping 3 'a 7 'b 10 'c)
            ) ;fxmapping-any?
) ;check-true
(check-false (fxmapping-any? (lambda (k v) (> k 100))
               (fxmapping 3 'a 7 'b)
             ) ;fxmapping-any?
) ;check-false

(check-report)
