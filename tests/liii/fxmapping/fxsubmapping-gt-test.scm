(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxsubmapping>
;; 获取键大于指定值的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping> fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 边界键（不包含）。
;;
;; 返回值
;; -----
;; 返回只包含键严格大于 key 的键值对的新 fxmapping。
;;
(let ((m (fxsubmapping> (fxmapping 0 'a 1 'b 2 'c 3 'd)
           1
         ) ;fxsubmapping>
      ) ;m
     ) ;
  (check-false (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-true (fxmapping-contains? m 3))
) ;let

(check-report)
