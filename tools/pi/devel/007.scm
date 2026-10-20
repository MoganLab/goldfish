(import (scheme base)
  (scheme file)
  (liii check)
  (liii subprocess)
  (liii json)
  (liii string)
  (liii list)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; [007] record 化·工具结构 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))

;; ============================================================================
;; 验收点 1: pi-tool-definition / pi-tool-result / pi-trunc-result
;;          为 define-record-type（谓词与访问器可用，含 assoc 兼容）
;; ============================================================================

;; 1.1 pi-tool-definition 构造器、谓词与字段访问器
(let* ((exec-fn (lambda (args . opts) "ok"))
       (params-val '(("type" . "object")))
       (def (pi-tool-definition "test-custom"
              "A test tool"
              params-val
              exec-fn
              "Custom tool snippet"
              '("Guideline 1" "Guideline 2")
            ) ;pi-tool-definition
       ) ;def
      ) ;
  (check-true (pi-tool-definition? def))
  (check-false (pi-tool-result? def))
  (check-false (pi-trunc-result? def))
  (check (pi-tool-definition-name def) => "test-custom")
  (check (pi-tool-definition-description def) => "A test tool")
  (check (pi-tool-definition-parameters def) => params-val)
  (check (procedure? (pi-tool-definition-execute def)) => #t)
  (check ((pi-tool-definition-execute def) '()) => "ok")
  (check (pi-tool-definition-snippet def) => "Custom tool snippet")
  (check (pi-tool-definition-guidelines def) => '("Guideline 1" "Guideline 2"))

  ;; assoc 兼容（string 与 symbol 键）
  (check (cdr (assoc 'name def)) => "test-custom")
  (check (cdr (assoc "name" def)) => "test-custom")
  (check (cdr (assoc 'description def)) => "A test tool")
  (check (cdr (assoc "description" def)) => "A test tool")
  (check (cdr (assoc 'parameters def)) => params-val)
  (check (cdr (assoc "parameters" def)) => params-val)
  (check (procedure? (cdr (assoc 'execute def))) => #t)
  (check (cdr (assoc 'snippet def)) => "Custom tool snippet")
  (check (cdr (assoc "snippet" def)) => "Custom tool snippet")
  (check (cdr (assoc 'guidelines def)) => '("Guideline 1" "Guideline 2"))
  (check (cdr (assoc "guidelines" def)) => '("Guideline 1" "Guideline 2"))

  ;; pi-tool-definition->alist 转换
  (let ((al (pi-tool-definition->alist def)))
    (check-true (pair? al))
    (check (cdr (assoc 'name al)) => "test-custom")
    (check (cdr (assoc 'description al)) => "A test tool")
  ) ;let
) ;let*

;; 1.2 get-tool-definition 对 7 个内置工具返回 record
(for-each
  (lambda (tool-name)
    (let ((def (get-tool-definition tool-name)))
      (check-true (pi-tool-definition? def))
      (check (pi-tool-definition-name def) => tool-name)
      (check (cdr (assoc 'name def)) => tool-name)
      (check-true (procedure? (pi-tool-definition-execute def)))
      (check-true (string? (pi-tool-definition-description def)))
    ) ;let
  ) ;lambda
  '("read" "bash" "edit" "write" "grep" "find" "ls")
) ;for-each

;; 1.3 pi-tool-result 构造器、谓词与访问器
(let* ((res-ok (pi-tool-result "Operation completed" #f))
       (res-err (pi-tool-result "Something went wrong" #t))
       (custom-content '#(((type . "text") (text . "custom"))))
       (res-vec (pi-tool-result custom-content #f))
      ) ;
  ;; res-ok
  (check-true (pi-tool-result? res-ok))
  (check-false (pi-tool-definition? res-ok))
  (check-false (pi-trunc-result? res-ok))
  (check (pi-tool-result-content res-ok)
    =>
    '#(((type . "text") (text . "Operation completed")))
  ) ;check
  (check (pi-tool-result-is-error res-ok) => #f)
  (check (pi-tool-result-isError res-ok) => #f)
  (check (cdr (assoc 'isError res-ok)) => #f)
  (check (cdr (assoc "isError" res-ok)) => #f)
  (check (cdr (assoc 'is-error res-ok)) => #f)
  (check (cdr (assoc "is-error" res-ok)) => #f)

  ;; res-err
  (check-true (pi-tool-result? res-err))
  (check (pi-tool-result-is-error res-err) => #t)
  (check (pi-tool-result-isError res-err) => #t)
  (check (cdr (assoc 'isError res-err)) => #t)
  (check (cdr (assoc "isError" res-err)) => #t)
  (check (cdr (assoc 'is-error res-err)) => #t)
  (check (cdr (assoc "is-error" res-err)) => #t)

  ;; res-vec
  (check-true (pi-tool-result? res-vec))
  (check (pi-tool-result-content res-vec) => custom-content)

  ;; pi-tool-result->alist 转换
  (let ((al (pi-tool-result->alist res-ok)))
    (check-true (pair? al))
    (check (cdr (assoc 'isError al)) => #f)
  ) ;let
) ;let*

;; 1.4 pi-trunc-result 构造器、谓词与全部访问器
(let ((tr (pi-trunc-result "sample text" #t 'bytes 50 1024 25 512 #t #f)))
  (check-true (pi-trunc-result? tr))
  (check-false (pi-tool-definition? tr))
  (check-false (pi-tool-result? tr))
  (check (pi-trunc-result-content tr) => "sample text")
  (check (pi-trunc-result-truncated tr) => #t)
  (check (pi-trunc-result-truncated? tr) => #t)
  (check (pi-trunc-result-truncated-by tr) => 'bytes)
  (check (pi-trunc-result-total-lines tr) => 50)
  (check (pi-trunc-result-total-bytes tr) => 1024)
  (check (pi-trunc-result-output-lines tr) => 25)
  (check (pi-trunc-result-output-bytes tr) => 512)
  (check (pi-trunc-result-first-line-exceeds-limit? tr) => #t)
  (check (pi-trunc-result-first-line-truncated tr) => #t)
  (check (pi-trunc-result-last-line-partial? tr) => #f)
  (check (pi-trunc-result-last-line-truncated tr) => #f)

  ;; assoc 兼容（string 与 symbol 键）
  (check (cdr (assoc 'content tr)) => "sample text")
  (check (cdr (assoc "content" tr)) => "sample text")
  (check (cdr (assoc 'truncated tr)) => #t)
  (check (cdr (assoc "truncated" tr)) => #t)
  (check (cdr (assoc 'truncated-by tr)) => 'bytes)
  (check (cdr (assoc "truncated-by" tr)) => 'bytes)
  (check (cdr (assoc 'total-lines tr)) => 50)
  (check (cdr (assoc "total-lines" tr)) => 50)
  (check (cdr (assoc 'total-bytes tr)) => 1024)
  (check (cdr (assoc "total-bytes" tr)) => 1024)
  (check (cdr (assoc 'output-lines tr)) => 25)
  (check (cdr (assoc "output-lines" tr)) => 25)
  (check (cdr (assoc 'output-bytes tr)) => 512)
  (check (cdr (assoc "output-bytes" tr)) => 512)
  (check (cdr (assoc 'first-line-exceeds-limit? tr)) => #t)
  (check (cdr (assoc 'first-line-truncated tr)) => #t)
  (check (cdr (assoc 'last-line-partial? tr)) => #f)
  (check (cdr (assoc 'last-line-truncated tr)) => #f)

  ;; pi-trunc-result->alist 转换
  (let ((al (pi-trunc-result->alist tr)))
    (check-true (pair? al))
    (check (cdr (assoc 'content al)) => "sample text")
  ) ;let
) ;let

;; ============================================================================
;; 验收点 2: 工具调用往返与截断行为零回归
;; ============================================================================

;; 2.1 正常调用 bash 工具
(let ((res (execute-tool-call "bash" '(("command" . "echo hello_007")))))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #f)
  (let ((c (pi-tool-result-content res)))
    (check-true (vector? c))
    (check (> (vector-length c) 0) => #t)
    (let ((first-block (vector-ref c 0)))
      (check (string-contains (cdr (assoc 'text first-block)) "hello_007") => #t)
    ) ;let
  ) ;let
) ;let

;; 2.2 正常调用 read 工具
(let ((res (execute-tool-call "read" '(("path"
                                        . "tools/pi/tests/fixtures/sample.txt")))
      ) ;res
     ) ;
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #f)
  (let ((c (pi-tool-result-content res)))
    (check-true (vector? c))
    (let ((first-block (vector-ref c 0)))
      (check (string-contains (cdr (assoc 'text first-block)) "hello sample") => #t)
    ) ;let
  ) ;let
) ;let

;; 2.3 未知工具调用错误处理
(let ((res-unknown (execute-tool-call "non_existent_tool_xyz" '())))
  (check-true (pi-tool-result? res-unknown))
  (check (pi-tool-result-is-error res-unknown) => #t)
  (check (cdr (assoc 'isError res-unknown)) => #t)
) ;let

;; 2.4 截断函数 pi-truncate-head 与 pi-truncate-tail 返回 record
(let ((head-res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check-true (pi-trunc-result? head-res))
  (check (pi-trunc-result-content head-res) => "line1\nline2\nline3")
  (check (pi-trunc-result-truncated head-res) => #f)
  (check (pi-trunc-result-output-lines head-res) => 3)
) ;let

(let ((tail-res (pi-truncate-tail "line1\nline2\nline3" 10 1000)))
  (check-true (pi-trunc-result? tail-res))
  (check (pi-trunc-result-content tail-res) => "line1\nline2\nline3")
  (check (pi-trunc-result-truncated tail-res) => #f)
) ;let

;; 2.5 超长内容截断行为验证
(let* ((long-text (string-join (map number->string (iota 100)) "\n"))
       (tr (pi-truncate-head long-text 10 10000))
      ) ;
  (check-true (pi-trunc-result? tr))
  (check (pi-trunc-result-truncated tr) => #t)
  (check (pi-trunc-result-output-lines tr) => 10)
  (check (pi-trunc-result-total-lines tr) => 100)
) ;let*

;; ============================================================================
;; 验收点 3: JSON 输出与 CLI 行为零变化
;; ============================================================================

;; 3.1 tool_execution_end 事件序列化 toolResult 结构
(let* ((tool-res (pi-tool-result "cmd output" #f))
       (event (pi-event-tool-execution-end "c1" "bash" tool-res #f))
       (json-str (pi-serialize-event event))
       (parsed (string->json json-str))
      ) ;
  (check (cdr (assoc "type" parsed)) => "tool_execution_end")
  (check (cdr (assoc "toolCallId" parsed)) => "c1")
  (check (cdr (assoc "toolName" parsed)) => "bash")
  (let ((res-json (cdr (assoc "result" parsed))))
    (check-true (pair? res-json))
    (check (cdr (assoc "isError" res-json)) => 'false)
    (let ((content-vec (cdr (assoc "content" res-json))))
      (check-true (vector? content-vec))
      (check (vector-length content-vec) => 1)
      (let ((block (vector-ref content-vec 0)))
        (check (cdr (assoc "type" block)) => "text")
        (check (cdr (assoc "text" block)) => "cmd output")
      ) ;let
    ) ;let
  ) ;let
) ;let*

;; 3.2 CLI 帮助输出可用
(let* ((ret (run "bin/gf pi --help" :timeout 30)))
  (check ret => 0)
) ;let*

;; ============================================================================
;; 验收点 4: bin/gf test 全量过
;; ============================================================================
(let* ((ret (run "bin/gf test tools/pi/tests/liii/" :timeout 120)))
  (check ret => 0)
) ;let*

(check-report)
