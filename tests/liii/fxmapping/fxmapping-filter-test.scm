(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-filter
;; 过滤满足谓词的键值对。
;;
;; 语法
;; ----
;; (fxmapping-filter pred fxmap)
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
;; 返回只包含满足 pred 的键值对的新 fxmapping。
;;
(let ((filtered (fxmapping-filter (lambda (k v) (> k 5))
                  (fxmapping 3 'a 7 'b 10 'c)
                ) ;fxmapping-filter
      ) ;filtered
     ) ;
  (check-false (fxmapping-contains? filtered 3)
  ) ;check-false
  (check-true (fxmapping-contains? filtered 7)
  ) ;check-true
  (check-true (fxmapping-contains? filtered 10)
  ) ;check-true
) ;let

(check-report)
