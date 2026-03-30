(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-keys
;; 获取所有键（升序）。
;;
;; 语法
;; ----
;; (fxmapping-keys fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按键升序排列的键列表。
;;
(check (fxmapping-keys (fxmapping 0 'a 1 'b 2 'c)) => '(0 1 2))

(check-report)
