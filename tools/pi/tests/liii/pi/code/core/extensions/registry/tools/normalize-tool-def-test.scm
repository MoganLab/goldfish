;; normalize-tool-def
;; 将工具定义统一规范化为扩展工具定义记录。
;;
;; 语法
;; ----
;; (normalize-tool-def def)
;;
;; 参数
;; ----
;; def : (or extension-tool-def alist)
;; 工具定义记录或字段 alist。
;;
;; 返回值
;; ----
;; extension-tool-def
;; 规范化后的工具定义记录。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry tools)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((tool (make-extension-tool-def "t" "d" '() (lambda (a c) 1))))
  (check (normalize-tool-def tool) => tool)
) ;let*

(let ((tool (normalize-tool-def (list (cons 'name "calc")
                                         (cons 'description "desc")
                                         (cons 'execute (lambda (a c) 1))
                                       ) ;list
              ) ;normalize-tool-def
            )
      ) ;
  (check-true (extension-tool-def? tool))
  (check (extension-tool-def-name tool) => "calc")
  (check (extension-tool-def-description tool) => "desc")
) ;let*

(check-report)
