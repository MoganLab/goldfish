;; register-prompt-injector!
;; 向注册表追加提示词注入器。
;;
;; 语法
;; ----
;; (register-prompt-injector! target injector)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; injector : (or string procedure)
;; 静态文本或动态注入过程。
;;
;; 返回值
;; ----
;; extension-registry
;; 更新后的注册表记录。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry prompt)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (register-prompt-injector! reg "INJECT")
  (check (length (extension-registry-prompt-injectors reg)) => 1)
  (check-catch 'type-error (register-prompt-injector! reg 123))
) ;let*

(check-report)
