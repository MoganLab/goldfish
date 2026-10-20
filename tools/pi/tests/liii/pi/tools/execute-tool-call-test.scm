(import (liii check)
  (liii pi tools)
  (liii pi tools record)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; 1. 未知工具调用返回 isError: #t 且错误信息明确
(let ((res (execute-tool-call "non_existent_tool" '())))
  (check-true (pi-tool-result? res))
  (check-true (pi-tool-result-is-error res))
  (check (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
         => "Tool non_existent_tool not found"))

;; 2. 动态注册工具执行并返回普通值（自动包装为 pi-tool-result）
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "echo_val"
               "Echo back argument"
               '((type . "object"))
               (lambda (args)
                 (let ((val (assoc "msg" args)))
                   (if val (cdr val) "empty"))))))
  (register-tool! api tool)
  (with-extension-registry api
    (lambda ()
      (let ((res (execute-tool-call "echo_val" '(("msg" . "hello")))))
        (check-true (pi-tool-result? res))
        (check-false (pi-tool-result-is-error res))
        (check (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
               => "hello")))))

;; 3. 动态注册工具直接返回 pi-tool-result 对象
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "result_tool"
               "Returns tool result directly"
               '()
               (lambda (args)
                 (pi-tool-result "custom formatted output" #f)))))
  (register-tool! api tool)
  (with-extension-registry api
    (lambda ()
      (let ((res (execute-tool-call "result_tool" '())))
        (check-true (pi-tool-result? res))
        (check-false (pi-tool-result-is-error res))
        (check (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
               => "custom formatted output")))))

;; 4. 扩展工具抛出异常时被安全捕获并包装为 isError: #t
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "broken_tool"
               "Always throws error"
               '()
               (lambda (args)
                 (error "something went wrong in extension")))))
  (register-tool! api tool)
  (with-extension-registry api
    (lambda ()
      (let ((res (execute-tool-call "broken_tool" '())))
        (check-true (pi-tool-result? res))
        (check-true (pi-tool-result-is-error res))
        (check (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
               => "something went wrong in extension")))))

;; 5. 扩展工具单参数 lambda 在收到额外 opts (如 :timeout) 时不崩溃
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "simple_fn"
               "Accepts single args parameter"
               '()
               (lambda (args) "safe"))))
  (register-tool! api tool)
  (with-extension-registry api
    (lambda ()
      (let ((res (execute-tool-call "simple_fn" '() :timeout 120 :opt "abc")))
        (check-true (pi-tool-result? res))
        (check-false (pi-tool-result-is-error res))
        (check (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
               => "safe")))))

(check-report)
