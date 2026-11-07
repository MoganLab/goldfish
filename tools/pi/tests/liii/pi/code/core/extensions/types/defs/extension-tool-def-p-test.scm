;; extension-tool-def?
;; 判断对象是否为扩展工具定义记录。
;;
;; 语法
;; ----
;; (extension-tool-def? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是工具定义记录时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(check-true
  (extension-tool-def? (make-extension-tool-def "t" "d" '() (lambda (a c) 1)))
) ;check-true
(check-false (extension-tool-def? #f))
(check-false (extension-tool-def? '()))
(check-false (extension-tool-def? (list (cons 'name "t"))))

(check-report)
