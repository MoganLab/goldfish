(import (liii check)
  (liii string)
  (liii pi code core tools)
  (liii pi ai tools record)
  (liii pi code core extensions)
  (liii pi ai types)
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

;; 6. 工具自身抛错时不得重跑：:timed-out? 标记必须生效（Issue #1189）
;;    旧实现在 catch 里不带 opts 重跑一次，超时标记被丢掉，命令照常执行，
;;    结果不是错误——这正是「工具失败被重跑」造成的假成功。
(let ((res (execute-tool-call "grep" '(("pattern" . "sample")) :timed-out? #t)))
  (check-true (pi-tool-result? res))
  (check-true (pi-tool-result-is-error res))
  (check-true (string-contains?
                (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
                "Command timed out"
              ) ;string-contains?
  ) ;check-true
) ;let

;; 7. 工具能接受 opts 时必须真的收到：扩展工具用 (lambda (args . opts) ...)
;;    声明可变参数，:timeout 等选项必须原样传下去（Issue #1189）
(let* ((api (make-extension-api))
       (tool (make-extension-tool-def
               "opts_tool"
               "Accepts keyword options"
               '()
               (lambda (args . opts)
                 (string-append "opts=" (number->string (length opts))))))
      ) ;
  (register-tool! api tool)
  (with-extension-registry api
    (lambda ()
      (let ((res (execute-tool-call "opts_tool" '() :timeout 120)))
        (check-true (pi-tool-result? res))
        (check-false (pi-tool-result-is-error res))
        (check (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
               => "opts=2")
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

;; 未知工具的 alist 错误视图
(let ((res (execute-tool-call "unknown_tool" '())))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #t)
  (check (cdr (assoc 'isError res)) => #t)
  (check (vector-ref (cdr (assoc 'content res)) 0)
    => '((type . "text") (text . "Tool unknown_tool not found"))))

;; 工具 throw 错误被捕获转成 isError 结果
(let ((res (execute-tool-call "read" '(("path" . "non_existent_file.xyz")))))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #t)
  (check (cdr (assoc 'isError res)) => #t)
  (let ((text (cdr (assoc 'text (vector-ref (cdr (assoc 'content res)) 0)))))
    (check-true (string-contains? text "Path not found"))))

;; 成功调用返回 isError: #f
(let ((test-file (if (file-exists? "liii/pi/code/cli.scm")
                   "liii/pi/code/cli.scm"
                   "tools/pi/liii/pi/code/cli.scm")))
  (let ((res (execute-tool-call "read" (list (cons "path" test-file) (cons "limit" 1)))))
    (check-true (pi-tool-result? res))
    (check (pi-tool-result-is-error res) => #f)
    (check (cdr (assoc 'isError res)) => #f)))

(check-report)
