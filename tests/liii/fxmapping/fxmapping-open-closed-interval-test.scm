(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-open-closed-interval
;; 获取半开半闭区间子映射 (low, high]。
;;
;; 语法
;; ----
;; (fxmapping-open-closed-interval fxmap low high)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; low, high : exact-integer
;; 区间边界（low 不包含，high 包含）。
;;
;; 返回值
;; -----
;; 返回只包含键在 (low, high] 范围内的键值对的新 fxmapping。
;;
(let ((m (fxmapping-open-closed-interval (fxmapping 0 'a 1 'b 2 'c 3 'd 4 'e)
           1
           4
         ) ;fxmapping-open-closed-interval
      ) ;m
     ) ;
  (check-false (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 2))
  (check-true (fxmapping-contains? m 4))
) ;let

(check-report)
