;; extension-flow-def?
;; 判断对象是否为扩展流程定义记录。
;;
;; 语法
;; ----
;; (extension-flow-def? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是流程定义记录时返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(check-true (extension-flow-def? (make-extension-flow-def 'f (lambda (p) #t) (lambda (a b c d) 0))))
(check-false (extension-flow-def? #f))
(check-false (extension-flow-def? '()))
(check-false (extension-flow-def? (list (cons 'name 'f))))

(check-report)
