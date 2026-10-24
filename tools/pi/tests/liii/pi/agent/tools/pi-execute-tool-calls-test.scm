(import (liii check)
  (liii string)
  (liii pi agent tools)
  (liii pi ai message)
  (liii pi tools)
)

(check-set-mode! 'report-failed)


;; 构造注入了工具执行器的 agent config
(define (make-agent-test-config)
  (list (cons 'execute-tool-call execute-tool-call)))


;; 1. 执行批量工具调用
(let* ((calls (list (list (cons 'id "call_1")
                          (cons 'name "bash")
                          (cons 'arguments (list (cons "command" "echo batch-1"))))
                    (list (cons 'id "call_2")
                          (cons 'name "bash")
                          (cons 'arguments (list (cons "command" "echo batch-2"))))))
       (res (pi-execute-tool-calls calls #f 10 (make-agent-test-config))))
  (check (length res) => 2)
  (let* ((m1 (car res))
         (m2 (cadr res))
         (c1 (pi-tool-result-message-content m1))
         (c2 (pi-tool-result-message-content m2)))
    (check (pi-text-block-text (car c1)) => "batch-1\n")
    (check (pi-text-block-text (car c2)) => "batch-2\n")))

;; 2. 传递 config 时正常批量执行
(let* ((calls (list (list (cons 'id "call_cfg")
                          (cons 'name "bash")
                          (cons 'arguments (list (cons "command" "echo cfg-val"))))))
       (cfg '((provider . "google")))
       (res (pi-execute-tool-calls calls #f 10 (cons (cons 'execute-tool-call execute-tool-call) cfg))))
  (check (length res) => 1)
  (let ((c (pi-tool-result-message-content (car res))))
    (check (pi-text-block-text (car c)) => "cfg-val\n")))

(check-report)
