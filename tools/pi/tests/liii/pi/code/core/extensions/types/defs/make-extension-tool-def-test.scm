;; make-extension-tool-def
;; 创建扩展工具定义记录。
;;
;; 语法
;; ----
;; (make-extension-tool-def name description parameters execute . opt-snippet opt-guidelines opt-default-active?)
;;
;; 参数
;; ----
;; name : string
;; 工具名称，非空字符串。
;;
;; description : string
;; 工具描述。
;;
;; parameters : any
;; 工具参数模式。
;;
;; execute : procedure
;; 工具执行过程。
;;
;; opt-snippet : string（可选）
;; 使用片段，缺省为空字符串。
;;
;; opt-guidelines : list（可选）
;; 使用指引列表，缺省为空列表。
;;
;; opt-default-active? : boolean（可选）
;; 是否默认启用，缺省为 #f。
;;
;; 返回值
;; ----
;; extension-tool-def
;; 新建的工具定义记录。

(import (scheme base)
        (liii check)
        (liii check)
        (liii pi code core extensions types defs)
) ;import

(check-set-mode! 'report-failed)

(let ((tool (make-extension-tool-def "t" "d" '((type . "object")) (lambda (a c) 1))))
  (check-true (extension-tool-def? tool))
  (check (extension-tool-def-name tool) => "t")
  (check (extension-tool-def-description tool) => "d")
  (check (extension-tool-def-parameters tool) => '((type . "object")))
  (check-true (procedure? (extension-tool-def-execute tool)))
  (check (extension-tool-def-snippet tool) => "")
  (check (extension-tool-def-guidelines tool) => '())
  (check (extension-tool-def-default-active? tool) => #f)
) ;let*

(check-report)
