;; bigcomplex-real
;; 获取 <bigcomplex> 的实部。
;;
;; 语法
;; ----
;; (bigcomplex-real z)
;;
;; 参数
;; ----
;; z : bigcomplex? 复数。
;;
;; 返回值
;; ----
;; real? 实部。

(import (liii check) (liii bigcomplex))

(check-set-mode! 'report-failed)

(check (bigcomplex-real (make-bigcomplex 3 4)) => 3)

(check-report)
