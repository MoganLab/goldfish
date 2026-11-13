;; bigcomplex-imag
;; 获取 <bigcomplex> 的虚部。
;;
;; 语法
;; ----
;; (bigcomplex-imag z)
;;
;; 参数
;; ----
;; z : bigcomplex? 复数。
;;
;; 返回值
;; ----
;; real? 虚部。

(import (liii check) (liii bigcomplex))

(check-set-mode! 'report-failed)

(check (bigcomplex-imag (make-bigcomplex 3 -4)) => -4)

(check-report)
