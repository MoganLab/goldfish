;; set-extension-registry-prompt-injectors!
;; 更新扩展注册表中的提示词注入器列表。
;;
;; 语法
;; ----
;; (set-extension-registry-prompt-injectors! reg value)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; value : list
;; 新的提示词注入器列表。
;;
;; 返回值
;; ----
;; 不返回
;; 直接更新注册表记录。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (set-extension-registry-prompt-injectors! reg '(a))
  (check (extension-registry-prompt-injectors reg) => '(a))
) ;let

(check-report)
