(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxsubmapping=
;; 获取指定键的子映射。
;;
;; 语法
;; ----
;; (fxsubmapping= fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 指定的键。
;;
;; 返回值
;; -----
;; 如果 key 存在于 fxmap 中，返回只包含该键值对的映射；
;; 否则返回空映射。
;;
(check (fxmapping-ref (fxsubmapping= (fxmapping 0 'a 1 'b) 0) 0 (lambda () #f)) => 'a)
(check-true (fxmapping-empty? (fxsubmapping= (fxmapping 0 'a 1 'b) 2)))

(check-report)
