;; extension-context-model
;; 读取扩展上下文中的模型标识。
;;
;; 语法
;; ----
;; (extension-context-model ctx)
;;
;; 参数
;; ----
;; ctx : extension-context
;; 扩展上下文记录。
;;
;; 返回值
;; ----
;; string 或 #f
;; 模型标识，未设置时为 #f。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types core)
) ;import

(check-set-mode! 'report-failed)

(check (extension-context-model (make-extension-context)) => #f)
(check (extension-context-model (make-extension-context #f "." "model-a"))
  =>
  "model-a"
) ;check

(check-report)
