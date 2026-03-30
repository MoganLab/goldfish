(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-values
;; 获取所有值（按键升序）。
;;
;; 语法
;; ----
;; (fxmapping-values fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按对应键升序排列的值列表。
;;
(check (fxmapping-values (fxmapping 0 'a 1 'b 2 'c)) => '(a b c))

(check-report)
