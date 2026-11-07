;; register-validator!
;; 向注册表追加参数校验过程。
;;
;; 语法
;; ----
;; (register-validator! target validator-proc)
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
  (register-validator! reg (lambda (parsed) #t))
  (check (length (extension-registry-validators reg)) => 1)
  (check-catch 'type-error (register-validator! reg "not-a-procedure"))
) ;let*

(check-report)
