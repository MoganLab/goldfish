(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-tool-definition-record-test.scm
;; 验证 pi-tool-definition record 构造器、谓词与访问器，以及对 assoc 的平滑兼容。

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
(check (pi-tool-definition-description test-def) => "A test tool for unit tests")
(check (pi-tool-definition-parameters test-def) => '(("type" . "object")))
(check (procedure? (pi-tool-definition-execute test-def)) => #t)
(check ((pi-tool-definition-execute test-def) '()) => "ok")
(check (pi-tool-definition-snippet test-def) => "Run test tool")
(check (pi-tool-definition-guidelines test-def) => '("Guideline 1" "Guideline 2"))

;; assoc 兼容访问 tool definition record
(check (cdr (assoc 'name test-def)) => "test-tool")
(check (cdr (assoc "name" test-def)) => "test-tool")
(check (cdr (assoc 'description test-def)) => "A test tool for unit tests")
(check (cdr (assoc "description" test-def)) => "A test tool for unit tests")
(check (cdr (assoc 'parameters test-def)) => '(("type" . "object")))
(check (cdr (assoc "parameters" test-def)) => '(("type" . "object")))
(check (procedure? (cdr (assoc 'execute test-def))) => #t)
(check (cdr (assoc 'snippet test-def)) => "Run test tool")
(check (cdr (assoc "snippet" test-def)) => "Run test tool")
(check (cdr (assoc 'guidelines test-def)) => '("Guideline 1" "Guideline 2"))
(check (cdr (assoc "guidelines" test-def)) => '("Guideline 1" "Guideline 2"))

;; 验证内置工具定义返回 record
(let ((read-def (get-tool-definition "read")))
  (check-true (pi-tool-definition? read-def))
  (check (pi-tool-definition-name read-def) => "read")
  (check (cdr (assoc 'name read-def)) => "read")
  (check (cdr (assoc 'snippet read-def)) => "Read file contents")
) ;let

(check-report)
