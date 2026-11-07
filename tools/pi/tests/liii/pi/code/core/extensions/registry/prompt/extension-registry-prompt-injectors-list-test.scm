;; extension-registry-prompt-injectors-list
;; 列出注册表中已注册的全部提示词注入器。
;;
;; 语法
;; ----
;; (extension-registry-prompt-injectors-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 提示词注入器列表。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry prompt)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-prompt-injectors-list reg) => '())
  (register-prompt-injector! reg "INJECT")
  (check (length (extension-registry-prompt-injectors-list reg)) => 1)
) ;let*

(check-report)
