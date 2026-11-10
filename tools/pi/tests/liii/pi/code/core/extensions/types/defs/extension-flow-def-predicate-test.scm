;; extension-flow-def-predicate
;; 读取流程定义中的判定过程。
;;
;; 语法
;; ----
;; (extension-flow-def-predicate flow)
;;
;; 参数
;; ----
;; flow : extension-flow-def
;; 流程定义记录。
;;
;; 返回值
;; ----
;; any
;; 判定过程。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((flow (make-extension-flow-def 'my-flow (lambda (parsed) #t) (lambda (p c m i) 0))
      ) ;flow
     ) ;
  (check-true (procedure? (extension-flow-def-predicate flow)))
) ;let

(check-report)
