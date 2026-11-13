;; make-bigcomplex
;; 构造 <bigcomplex> 复数实例。
;;
;; 语法
;; ----
;; (make-bigcomplex real imag)
;;
;; 参数
;; ----
;; real : real? 实部。
;; imag : real? 虚部。
;;
;; 返回值
;; ----
;; bigcomplex? 构造的复数实例。

(import (liii check) (liii bigcomplex))

(check-set-mode! 'report-failed)

(check (bigcomplex->string (make-bigcomplex 3 4)) => "3+4i")

(check-report)
