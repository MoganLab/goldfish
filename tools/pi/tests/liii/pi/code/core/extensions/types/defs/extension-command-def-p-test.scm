;; extension-command-def?
;; 判断对象是否为扩展命令定义记录。
;;
;; 语法
;; ----
;; (extension-command-def? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是命令定义记录时返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(check-true (extension-command-def? (make-extension-command-def "c" "d" (lambda (a x) 1))))
(check-false (extension-command-def? #f))
(check-false (extension-command-def? '()))
(check-false (extension-command-def? (list (cons 'name "c"))))

(check-report)
