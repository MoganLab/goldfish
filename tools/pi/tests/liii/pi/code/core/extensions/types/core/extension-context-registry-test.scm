;; extension-context-registry
;; 读取扩展上下文绑定的注册表记录。
;;
;; 语法
;; ----
;; (extension-context-registry ctx)
;;
;; 参数
;; ----
;; ctx : extension-context
;; 扩展上下文记录。
;;
;; 返回值
;; ----
;; extension-registry
;; 绑定的注册表记录。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (ctx (make-extension-context #f "." #f reg))
      ) ;
  (check (extension-context-registry ctx) => reg)
) ;let*

(check-report)
