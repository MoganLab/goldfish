;; normalize-flow-def
;; 将流程定义统一规范化为扩展流程定义记录。
;;
;; 语法
;; ----
;; (normalize-flow-def def)
;;
;; 参数
;; ----
;; def : (or extension-flow-def alist)
;; 流程定义记录或字段 alist。
;;
;; 返回值
;; ----
;; extension-flow-def
;; 规范化后的流程定义记录。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry flows)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((flow (make-extension-flow-def 'f (lambda (p) #t) (lambda (a b c d) 0))))
  (check (normalize-flow-def flow) => flow)
) ;let*

(let ((flow (normalize-flow-def (list (cons 'name 'my-flow)
                                          (cons 'predicate (lambda (p) #t))
                                          (cons 'handler (lambda (a b c d) 0))
                                        ) ;list
              ) ;normalize-flow-def
            )
      ) ;
  (check-true (extension-flow-def? flow))
  (check (extension-flow-def-name flow) => 'my-flow)
) ;let*

(check-report)
