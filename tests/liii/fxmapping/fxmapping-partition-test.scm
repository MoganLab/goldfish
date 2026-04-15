(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-partition
;; 按谓词分割映射。
;;
;; 语法
;; ----
;; (fxmapping-partition pred fxmap)
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
;; 返回两个值：满足 pred 的键值对组成的新映射，和不满足的组成的新映射。
;;
(let-values (((yes no)
              (fxmapping-partition (lambda (k v) (> k 5))
                (fxmapping 3 'a 7 'b 10 'c)
              ) ;fxmapping-partition
             ) ;
            ) ;
  (check-true (fxmapping-contains? yes 7))
  (check-false (fxmapping-contains? yes 3)
  ) ;check-false
  (check-true (fxmapping-contains? no 3))
  (check-false (fxmapping-contains? no 7))
) ;let-values

(check-report)
