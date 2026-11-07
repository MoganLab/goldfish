;; extension-flow-def-priority
;; 读取流程定义中的优先级数值。
;;
;; 语法
;; ----
;; (extension-flow-def-priority flow)
;;
;; 参数
;; ----
;; flow : extension-flow-def
;; 流程定义记录。
;;
;; 返回值
;; ----
;; any
;; 优先级数值。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((flow (make-extension-flow-def 'my-flow (lambda (parsed) #t) (lambda (p c m i) 0))))
  (check (extension-flow-def-priority flow) => 0))

(check-report)
