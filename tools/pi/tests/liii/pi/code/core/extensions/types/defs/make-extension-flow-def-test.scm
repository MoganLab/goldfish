;; make-extension-flow-def
;; 创建扩展流程定义记录。
;;
;; 语法
;; ----
;; (make-extension-flow-def name predicate handler . opt-local? opt-priority)
;;
;; 参数
;; ----
;; name : string 或 symbol
;; 流程名。
;;
;; predicate : procedure
;; 流程判定过程。
;;
;; handler : procedure
;; 流程处理过程。
;;
;; opt-local? : boolean（可选）
;; 是否为本地流程，缺省为 #f。
;;
;; opt-priority : number（可选）
;; 优先级，缺省为 0。
;;
;; 返回值
;; ----
;; extension-flow-def
;; 新建的流程定义记录。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((flow (make-extension-flow-def 'my-flow (lambda (parsed) #t) (lambda (p c m i) 0))
      ) ;flow
     ) ;
  (check-true (extension-flow-def? flow))
  (check (extension-flow-def-name flow) => 'my-flow)
  (check-true (procedure? (extension-flow-def-predicate flow)))
  (check-true (procedure? (extension-flow-def-handler flow)))
  (check (extension-flow-def-local? flow) => #f)
  (check (extension-flow-def-priority flow) => 0)
) ;let

(check-report)
