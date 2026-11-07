;; register-tool!
;; 向注册表注册工具定义，同名工具按名称覆盖。
;;
;; 语法
;; ----
;; (register-tool! target tool-def)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; tool-def : (or extension-tool-def alist)
;; 工具定义。
;;
;; 返回值
;; ----
;; extension-tool-def
;; 规范化后注册的工具定义记录。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry tools)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry))
       (tool
         (register-tool! reg (make-extension-tool-def "calc" "d" '() (lambda (a c) 1)))
       ) ;tool
      ) ;
  (check (length (extension-registry-tools reg)) => 1)
  (check (extension-tool-def-name tool) => "calc")
) ;let*

(check-report)
