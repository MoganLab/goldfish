(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-empty?
;; 检查映射是否为空。
;;
;; 语法
;; ----
;; (fxmapping-empty? fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 如果 fxmap 不包含任何键值对，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-empty? (fxmapping))
) ;check-true
(check-false (fxmapping-empty? (fxmapping 0 'a))
) ;check-false

(check-report)
