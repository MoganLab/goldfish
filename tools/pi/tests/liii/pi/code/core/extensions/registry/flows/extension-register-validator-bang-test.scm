;; extension-register-validator!
;; 向注册表追加参数校验过程（register-validator! 的别名）。
;;
;; 语法
;; ----
;; (extension-register-validator! target validator-proc)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; validator-proc : procedure
;; 校验过程。
;;
;; 返回值
;; ----
;; procedure
;; 注册的校验过程。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flows)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (extension-register-validator! reg (lambda (parsed) #t))
  (check (length (extension-registry-validators reg)) => 1)
) ;let

(check-report)
