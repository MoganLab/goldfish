;; make-extension-command-def
;; 创建扩展命令定义记录。
;;
;; 语法
;; ----
;; (make-extension-command-def name description handler . opt-params)
;;
;; 参数
;; ----
;; name : string
;; 命令名，非空字符串。
;;
;; description : string
;; 命令描述。
;;
;; handler : procedure
;; 命令处理过程。
;;
;; opt-params : list（可选）
;; 命令参数描述列表，缺省为空列表。
;;
;; 返回值
;; ----
;; extension-command-def
;; 新建的命令定义记录。

(import (scheme base)
  (liii check)
  (liii check)
  (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((cmd (make-extension-command-def "c" "d" (lambda (a x) 1))))
  (check-true (extension-command-def? cmd))
  (check (extension-command-def-name cmd) => "c")
  (check (extension-command-def-description cmd) => "d")
  (check-true (procedure? (extension-command-def-handler cmd)))
  (check (extension-command-def-params cmd) => '())
) ;let

(check-report)
