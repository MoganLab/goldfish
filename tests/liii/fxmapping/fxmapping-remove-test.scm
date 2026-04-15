(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-remove
;; 移除满足谓词的键值对。
;;
;; 语法
;; ----
;; (fxmapping-remove pred fxmap)
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
;; 返回不包含满足 pred 的键值对的新 fxmapping。
;;
(let ((removed (fxmapping-remove (lambda (k v) (> k 5))
                 (fxmapping 3 'a 7 'b 10 'c)
               ) ;fxmapping-remove
      ) ;removed
     ) ;
  (check-true (fxmapping-contains? removed 3)
  ) ;check-true
  (check-false (fxmapping-contains? removed 7)
  ) ;check-false
  (check-false (fxmapping-contains? removed 10)
  ) ;check-false
) ;let

(check-report)
