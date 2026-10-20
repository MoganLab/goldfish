(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi)
  (liii pi types)
  (liii pi message)
  (liii pi agent tools)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; [074] 验收脚本：上下文与生命周期拦截：Prompt 注入与 Tool 拦截

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-074-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (when (file-exists? tmp)
        (delete-file tmp)
      ) ;when
      ret
    ) ;let
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 074 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/agent.scm" "tools/pi/liii/pi/agent/tools.scm"
    "tools/pi/liii/pi/cli.scm" "tools/pi/liii/pi/code-review/auto.scm"
    "tools/pi/liii/pi/extension.scm" "tools/pi/liii/pi/extension/builtin.scm"
    "tools/pi/liii/pi/extension/builtin/prompts.scm"
    "tools/pi/liii/pi/extension/builtin/tdd.scm"
    "tools/pi/liii/pi/extension/event.scm"
    "tools/pi/liii/pi/extension/registry.scm"
    "tools/pi/liii/pi/extension/registry/prompt.scm"
    "tools/pi/liii/pi/extension/types.scm"
    "tools/pi/liii/pi/extension/types/core.scm"
    "tools/pi/liii/pi/modes/flow.scm" "tools/pi/liii/pi/prompt.scm"
    "tools/pi/tests/liii/pi/code-review/auto/pi-build-review-injection-test.scm"
    "tools/pi/tests/liii/pi/extension/prompt-injector-test.scm"
    "tools/pi/tests/liii/pi/extension/tool-call-block-test.scm"
    "tools/pi/tests/liii/pi/extension/tool-call-terminate-test.scm"
    "tools/pi/tests/liii/pi/extension/tool-result-event-test.scm"
    "tools/pi/devel/074.scm")
) ;for-each

(display "=== 验收点 3: 核心解耦与硬编码 import / 分支移除校验 ===\n"
) ;display
(let ((t-src (path-read-text (path "tools/pi/liii/pi/agent/tools.scm")))
      (p-src (path-read-text (path "tools/pi/liii/pi/prompt.scm")))
      (f-src (path-read-text (path "tools/pi/liii/pi/modes/flow.scm")))
     ) ;
  (check-false (string-contains t-src "pi-tdd-write-gate"))
  (check-false (string-contains t-src "pi-tdd-observe-tool!"))
  (check-false (string-contains t-src "red-green"))
  (check-false (string-contains t-src "blueprint"))
  (check-false (string-contains t-src "compass"))
  (check-false (string-contains p-src "tdd_test_run"))
  (check-false (string-contains p-src "codebase_map"))
  (check-false (string-contains p-src "code_tour"))
  (check-false (string-contains f-src "pi-tdd-turn-system-prompt"))
  (check-false (string-contains f-src "pi-blueprint-turn-system-prompt"))
  (check-false (string-contains f-src "pi-compass-turn-system-prompt"))
) ;let

(display "=== 验收点 4: Prompt 注入器机制与动态装配（含真实 record tool-call） ===\n"
) ;display
(let* ((reg (make-extension-registry))
       (tc1 (pi-tool-call-block "c1" "write" '(("path" . "a.scm")) :thought-signature "s1")
       ) ;tc1
       (tc2 (pi-tool-call-block "c2" "edit" '(("path" . "b.scm")) :thought-signature "s2")
       ) ;tc2
       (tc-records (list tc1 tc2))
      ) ;
  (check-true (pi-tool-call-block? tc1))
  (check (pi-tool-call-block-id tc1) => "c1")
  (check (pi-tool-call-block-name tc1) => "write")
  (check (pi-tool-call-block-arguments tc1) => '(("path" . "a.scm")))
  (check (pi-tool-call-block-thought-signature tc1) => "s1")
  (register-prompt-injector! reg "STATIC_SEC")
  (register-prompt-injector! reg (lambda () "THUNK_SEC"))
  (register-prompt-injector! reg
    (lambda (calls)
      (if (null? calls)
        "NO_CALLS"
        (string-append "N:"
          (number->string (length calls))
          ";T:"
          (pi-tool-call-block-name (car calls))
        ) ;string-append
      ) ;if
    ) ;lambda
  ) ;register-prompt-injector!
  (check (extension-registry-collect-prompt-injectors reg '())
    =>
    '("STATIC_SEC" "THUNK_SEC" "NO_CALLS")
  ) ;check
  (check (extension-registry-collect-prompt-injectors reg tc-records)
    =>
    '("STATIC_SEC" "THUNK_SEC" "N:2;T:write")
  ) ;check
) ;let*

(display "=== 验收点 5: tool_call 拦截钩子与 Input Override ===\n")
(let* ((reg (make-extension-registry)) (called? #f) (seen-input #f))
  (register-tool! reg
    (make-extension-tool-def "t_audit"
      "Audit"
      '()
      (lambda (args) (set! called? #t) (set! seen-input args) "ok")
    ) ;make-extension-tool-def
  ) ;register-tool!
  (pi-on! reg
    'tool_call
    (lambda (p)
      (if (string=? (cdr (assq 'tool p)) "t_audit")
        (list 'override '((mode . "safe")))
        'continue
      ) ;if
    ) ;lambda
  ) ;pi-on!
  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "c1") (name . "t_audit") (arguments (mode . "raw"))))
             (res (pi-execute-one-tool-call call #f 10))
            ) ;
        (check called? => #t)
        (check seen-input => '((mode . "safe")))
        (check (pi-tool-result-message-is-error res) => #f)
      ) ;let*
    ) ;lambda
  ) ;with-extension-registry
  (pi-on! reg
    'tool_call
    (lambda (p)
      (if (string=? (cdr (assq 'tool p)) "t_audit")
        '((block . #t) (reason . "Forbidden"))
        'continue
      ) ;if
    ) ;lambda
  ) ;pi-on!
  (with-extension-registry reg
    (lambda ()
      (set! called? #f)
      (let* ((call '((id . "c2") (name . "t_audit") (arguments)))
             (res (pi-execute-one-tool-call call #f 10))
            ) ;
        (check called? => #f)
        (check (pi-tool-result-message-is-error res) => #t)
        (check (pi-extract-tool-result-text res) => "Forbidden")
      ) ;let*
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(display "=== 验收点 6: 对齐原生 pi 的 terminate 语义与工具批次截断 ===\n"
) ;display
(let* ((reg (make-extension-registry)) (t2-called? #f))
  (register-tool! reg
    (make-extension-tool-def "s1" "S1" '() (lambda (a) "s1 ok"))
  ) ;register-tool!
  (register-tool! reg
    (make-extension-tool-def "s2"
      "S2"
      '()
      (lambda (a) (set! t2-called? #t) "s2 ok")
    ) ;make-extension-tool-def
  ) ;register-tool!
  (pi-on! reg
    'tool_call
    (lambda (p)
      (if (string=? (cdr (assq 'tool p)) "s1")
        '((block . #t) (reason . "Stop batch") (terminate . #t))
        'continue
      ) ;if
    ) ;lambda
  ) ;pi-on!
  (with-extension-registry reg
    (lambda ()
      (let ((calls '(((id . "c1") (name . "s1") (arguments))
                     ((id . "c2") (name . "s2") (arguments)))
            ) ;calls
           ) ;
        (let-values (((res terminated?) (pi-execute-tool-calls-with-status calls #f 10)))
          (check terminated? => #t)
          (check (length res) => 2)
          (check (pi-tool-result-message-is-error (car res)) => #t)
          (check (pi-extract-tool-result-text (car res)) => "Stop batch")
          (check (pi-tool-result-message-is-error (cadr res)) => #t)
          (check t2-called? => #f)
          (check (pi-extract-tool-result-text (cadr res))
            =>
            "Tool call skipped: previous tool call terminated the run"
          ) ;check
        ) ;let-values
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(display "=== 验收点 7: tool_result 生命周期事件派发 ===\n")
(let* ((reg (make-extension-registry)) (events '()))
  (register-tool! reg
    (make-extension-tool-def "t_q" "Q" '() (lambda (a) "answer:42"))
  ) ;register-tool!
  (pi-on! reg 'tool_result (lambda (p) (set! events (cons p events))))
  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "cq") (name . "t_q") (arguments (q . 1))))
             (res (pi-execute-one-tool-call call #f 10))
            ) ;
        (check (length events) => 1)
        (let ((ev (car events)))
          (check (cdr (assq 'name ev)) => "t_q")
          (check (cdr (assq 'id ev)) => "cq")
          (check (cdr (assq 'input ev)) => '((q . 1)))
          (check (cdr (assq 'result ev)) => "answer:42")
          (check (cdr (assq 'isError ev)) => #f)
        ) ;let
      ) ;let*
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(display "=== 验收点 8: 全套相关单元测试与回归套件执行 ===\n")
(check (%run-ok? "t-ext"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/extension/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-agent" "timeout 120 bin/gf test tools/pi/tests/liii/pi/agent/" 120)
  =>
  0
) ;check
(check (%run-ok? "t-modes" "timeout 120 bin/gf test tools/pi/tests/liii/pi/modes/" 120)
  =>
  0
) ;check
(check (%run-ok? "t-all-pi" "timeout 300 bin/gf test tools/pi/tests/liii/pi/" 300)
  =>
  0
) ;check

(check-report)
