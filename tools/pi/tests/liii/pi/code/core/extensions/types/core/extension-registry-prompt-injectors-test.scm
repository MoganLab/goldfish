;; extension-registry-prompt-injectors
;; 读取扩展注册表中的提示词注入器列表。
;;
;; 语法
;; ----
;; (extension-registry-prompt-injectors reg)
;;
;; 参数
;; ----
;; reg : extension-registry
;; 扩展注册表记录。
;;
;; 返回值
;; ----
;; list
;; 提示词注入器列表。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-prompt-injectors reg) => '()))

(check-report)
