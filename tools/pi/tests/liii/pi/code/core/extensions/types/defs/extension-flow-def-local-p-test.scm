;; extension-flow-def-local?
;; 读取流程定义中的是否为本地流程。
;;
;; 语法
;; ----
;; (extension-flow-def-local? flow)
;;
;; 参数
;; ----
;; flow : extension-flow-def
;; 流程定义记录。
;;
;; 返回值
;; ----
;; any
;; 是否为本地流程。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((flow (make-extension-flow-def 'my-flow (lambda (parsed) #t) (lambda (p c m i) 0))))
  (check (extension-flow-def-local? flow) => #f))

(check-report)
