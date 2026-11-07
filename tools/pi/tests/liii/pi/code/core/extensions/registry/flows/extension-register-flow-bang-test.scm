;; extension-register-flow!
;; 向注册表注册流程定义（register-flow! 的别名）。
;;
;; 语法
;; ----
;; (extension-register-flow! target flow-def)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; flow-def : (or extension-flow-def alist)
;; 流程定义。
;;
;; 返回值
;; ----
;; extension-flow-def
;; 规范化后注册的流程定义记录。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry flows)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (flow
         (extension-register-flow! reg
           (make-extension-flow-def 'my-flow (lambda (p) #t) (lambda (a b c d) 0))
         ) ;extension-register-flow!
       ) ;flow
      ) ;
  (check (length (extension-registry-flows reg)) => 1)
  (check (extension-flow-def-name flow) => 'my-flow)
) ;let*

(check-report)
