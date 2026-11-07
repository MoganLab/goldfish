;; find-extension-tool
;; 按名称查找注册表中已注册的工具。
;;
;; 语法
;; ----
;; (find-extension-tool target name)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; name : string
;; 工具名称。
;;
;; 返回值
;; ----
;; (or extension-tool-def #f)
;; 匹配的工具定义记录，未找到时返回 #f。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry tools)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* (
       (reg (make-extension-registry))
       (tool (make-extension-tool-def "calc" "d" '() (lambda (a c) 1))))
  (register-tool! reg tool)
  (check (find-extension-tool reg "calc") => tool)
  (check (find-extension-tool reg "missing") => #f)
) ;let*

(check-report)
