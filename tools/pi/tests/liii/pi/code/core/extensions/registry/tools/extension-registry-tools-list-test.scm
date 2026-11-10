;; extension-registry-tools-list
;; 列出注册表中已注册的全部工具。
;;
;; 语法
;; ----
;; (extension-registry-tools-list target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; list
;; 工具定义记录列表。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry tools)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (extension-registry-tools-list reg) => '())
  (register-tool! reg (make-extension-tool-def "calc" "d" '() (lambda (a c) 1)))
  (check (length (extension-registry-tools-list reg)) => 1)
) ;let

(check-report)
