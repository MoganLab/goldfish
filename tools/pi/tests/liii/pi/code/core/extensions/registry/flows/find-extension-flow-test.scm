;; find-extension-flow
;; 按流程名查找注册表中已注册的流程。
;;
;; 语法
;; ----
;; (find-extension-flow target name)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; name : (or symbol string)
;; 流程名。
;;
;; 返回值
;; ----
;; (or extension-flow-def #f)
;; 匹配的流程定义记录，未找到时返回 #f。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry flows)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* (
       (reg (make-extension-registry))
       (flow (make-extension-flow-def 'my-flow (lambda (p) #t) (lambda (a b c d) 0))))
  (register-flow! reg flow)
  (check (find-extension-flow reg 'my-flow) => flow)
  (check (find-extension-flow reg 'missing) => #f)
) ;let*

(check-report)
