(import (liii check) (liii pi ai event) (liii pi ai tools record) (liii pi code core tools))

(check-set-mode! 'report-failed)

;; pi-tool-result-record-test.scm
;; 验证 pi-tool-result record 构造器、谓词与访问器，以及对 assoc 的平滑兼容。

(define res1
  (pi-tool-result "hello world" #f)
) ;define

(check-true (pi-tool-result? res1))
(check (pi-tool-result-content res1) => '#(((type . "text") (text . "hello world"))))
(check (pi-tool-result-is-error res1) => #f)
(check (pi-tool-result-isError res1) => #f)

;; assoc 兼容访问 tool result record
(check (cdr (assoc 'content res1)) => '#(((type . "text") (text . "hello world"))))
(check (cdr (assoc "content" res1)) => '#(((type . "text") (text . "hello world"))))
(check (cdr (assoc 'isError res1)) => #f)
(check (cdr (assoc "isError" res1)) => #f)
(check (cdr (assoc 'is-error res1)) => #f)
(check (cdr (assoc "is-error" res1)) => #f)

(define res-err
  (pi-tool-result "failed with error" #t)
) ;define
(check-true (pi-tool-result? res-err))
(check (pi-tool-result-is-error res-err) => #t)
(check (cdr (assoc 'isError res-err)) => #t)
(check (cdr (assoc 'is-error res-err)) => #t)

;; 支持直接传入 vector content
(define custom-content '#(((type . "text") (text . "custom"))))
(define res-custom
  (pi-tool-result custom-content #f)
) ;define
(check-true (pi-tool-result? res-custom))
(check (pi-tool-result-content res-custom) => custom-content)

;; 验证 execute-tool-call 返回 record
(let ((res (execute-tool-call "unknown_tool" '())))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #t)
  (check (cdr (assoc 'isError res)) => #t)
) ;let

(check-report)
