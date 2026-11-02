;; pi-tool-definition-test.scm
;; pi-tool-definition
;; 构造工具定义 record 对象。
;;
;; 语法
;; ----
;; (pi-tool-definition name description parameters execute)
;; (pi-tool-definition name description parameters execute snippet guidelines)
;;
;; 参数
;; ----
;; name : string?
;; 工具名称。
;;
;; description : string?
;; 工具描述。
;;
;; parameters : any?
;; 参数模式。
;;
;; execute : procedure?
;; 执行回调过程。
;;
;; snippet : string?
;; 可选，代码片段提示。
;;
;; guidelines : list?
;; 可选，使用指南列表。
;;
;; 返回值
;; ----
;; pi-tool-definition?
;; 工具定义 record 实例。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(define test-def
  (pi-tool-definition "test-tool"
    "A test tool for unit tests"
    '(("type" . "object"))
    (lambda (args . opts) "ok")
    "Run test tool"
    '("Guideline 1" "Guideline 2")
  ) ;pi-tool-definition
) ;define

(check-true (pi-tool-definition? test-def))
(check (pi-tool-definition-name test-def) => "test-tool")
(check (pi-tool-definition-description test-def)
  =>
  "A test tool for unit tests"
) ;check
(check (procedure? (pi-tool-definition-execute test-def)) => #t)
(check (pi-tool-definition-snippet test-def) => "Run test tool")
(check (pi-tool-definition-guidelines test-def)
  =>
  '("Guideline 1" "Guideline 2")
) ;check

(check-report)
