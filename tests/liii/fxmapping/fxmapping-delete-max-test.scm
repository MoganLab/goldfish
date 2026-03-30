(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-delete-max
;; 删除键最大的键值对。
;;
;; 语法
;; ----
;; (fxmapping-delete-max fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含键最大的键值对。
;;
(let ((m (fxmapping 0 'a 1 'b 2 'c)))
  (check-false (fxmapping-contains? (fxmapping-delete-max m) 2))
  (check-true (fxmapping-contains? (fxmapping-delete-max m) 1))
) ;let

(check-report)
