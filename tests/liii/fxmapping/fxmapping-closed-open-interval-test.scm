(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-closed-open-interval
;; 获取半闭半开区间子映射 [low, high)。
;;
;; 语法
;; ----
;; (fxmapping-closed-open-interval fxmap low high)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; low, high : exact-integer
;; 区间边界（low 包含，high 不包含）。
;;
;; 返回值
;; -----
;; 返回只包含键在 [low, high) 范围内的键值对的新 fxmapping。
;;
(let ((m (fxmapping-closed-open-interval (fxmapping 0 'a 1 'b 2 'c 3 'd 4 'e) 1 4)))
  (check-true (fxmapping-contains? m 1))
  (check-true (fxmapping-contains? m 3))
  (check-false (fxmapping-contains? m 4))
) ;let

(check-report)
