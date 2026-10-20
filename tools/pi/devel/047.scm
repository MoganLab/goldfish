(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi bash)
  (liii pi args)
  (liii pi blueprint graph)
  (liii pi blueprint types)
  (liii pi blueprint state)
  (liii pi blueprint transition)
  (liii pi blueprint store)
) ;import

(check-set-mode! 'report-failed)

;; [047] 验收脚本：blueprint 计划命令
;; 逐条对应 tools/pi/devel/047.md 验收点 1-9：参数解析与互斥、stub 端到端
;; 计划生成/依赖跟踪/门禁推进/上下文注入、输出分流、边界安全与门禁回归。
;; 辅助函数见 047-support.scm。

(load "tools/pi/devel/047-support.scm")

(%cleanup!)
(%check-arg-parsing!)


(display "=== 验收点 2-8: stub 端到端 ===\n")
(%setup-dir! %tmp1)
(%setup-dir! %tmp2)
(let* ((port (%start-stub!))
       (base (string-append "http://127.0.0.1:" (number->string port)))
       (env-plan (string-append base "/blueprint-plan"))
       (env-next (string-append base "/blueprint-next"))
       (fixture-before (vector->list (path-list (path %fixture))))
      ) ;
  (dynamic-wind (lambda () #f)
    (lambda ()
      ;; 互斥错误与前置校验：真实 CLI 退出码 1 + 可读错误。
      (for-each
        (lambda (pair)
          (let ((res (%run-pi %tmp2 base (cons "--print" (cdr pair)))))
            (check (car res) => 1)
            (check-true (string-contains? (caddr res) (car pair)))
          ) ;let
        ) ;lambda
        '(("Only one of --blueprint" "--blueprint" "x" "--plan-status")
          ("--blueprint and --plan-*" "--blueprint" "x" "--simplify")
          ("--approve requires --plan-verify" "--approve")
          ("--blueprint requires an objective" "--blueprint"))
      ) ;for-each

      ;; 无活动计划：四条本地命令退出码 0，提示语一致。
      (for-each
        (lambda (flag)
          (let ((res (%run-pi %tmp2 base (list "--print" flag))))
            (check (car res) => 0)
            (check-true (string-contains? (cadr res) "No active blueprint."))
          ) ;let
        ) ;lambda
        '("--plan-status" "--plan-next" "--plan-verify" "--plan-abandon")
      ) ;for-each

      ;; JSON 模式生成计划：stdout 纯 JSONL，计划摘要写 stderr。
      (let ((res (%run-pi %tmp1
                   env-plan
                   '("--mode" "json" "--blueprint" "Add two numbers"
                     "--no-session" "--timeout" "30" "--session-timeout" "120")
                 ) ;%run-pi
            ) ;res
           ) ;
        (check (car res) => 0)
        (%check-jsonl (cadr res))
        (check-true (string-contains? (caddr res) "# Blueprint: Add two numbers"))
        (check-false (string-contains? (cadr res) "# Blueprint"))
        (check-true (path-exists? (path (%state-file %tmp1))))
        (check-true
          (path-exists? (path (pi-blueprint-plan-path (%state-file %tmp1))))
        ) ;check-true
        (check-true
          (path-exists? (path (pi-blueprint-history-path (%state-file %tmp1))))
        ) ;check-true
      ) ;let

      ;; 依赖跟踪：无依赖 pending、依赖未满足 blocked、就绪任务为 1.1。
      (let* ((bp (%load-bp %tmp1))
             (tasks (pi-blueprint-all-tasks bp))
             (next (pi-blueprint-get-next-task bp))
            ) ;
        (check (pi-task-status (pi-blueprint-find-task tasks "1.1")) => "pending")
        (check (pi-task-status (pi-blueprint-find-task tasks "1.2")) => "blocked")
        (check (pi-task-status (pi-blueprint-find-task tasks "2.1")) => "blocked")
        (check (pi-task-id next) => "1.1")
      ) ;let*

      ;; 放弃后用 print 模式重新生成，stdout 直接输出计划与进度。
      (let ((res (%run-pi %tmp1 base '("--print" "--plan-abandon"))))
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "Blueprint abandoned."))
      ) ;let
      (let ((res (%run-pi %tmp1
                   env-plan
                   '("--print" "--blueprint" "Add two numbers" "--no-session"
                     "--timeout" "30" "--session-timeout" "120")
                 ) ;%run-pi
            ) ;res
           ) ;
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "# Blueprint: Add two numbers"))
        (check-true (string-contains? (cadr res) "Progress: 0/4 tasks (0%)"))
      ) ;let

      ;; plan-status 输出进度与计划。
      (let ((res (%run-pi %tmp1 base '("--print" "--plan-status"))))
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "Progress: 0/4 tasks (0%)"))
        (check-true (string-contains? (cadr res) "# Blueprint: Add two numbers"))
      ) ;let

      ;; plan-next：注入 Active Blueprint 上下文并调用 blueprint_update 完成 1.1。
      (let ((res (%run-pi %tmp1
                   env-next
                   '("--print" "--plan-next" "--no-session" "--timeout" "30"
                     "--session-timeout" "120")
                 ) ;%run-pi
            ) ;res
           ) ;
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "## Next Task: 1.1 - Add helper"))
        (check-true (string-contains? (cadr res) "Task done."))
        (check-false (string-contains? (string-append (cadr res) (caddr res)) "MISSING_INJECTION")
        ) ;check-false
        (check
          (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks (%load-bp %tmp1)) "1.1")
          ) ;pi-task-status
          =>
          "completed"
        ) ;check
      ) ;let

      ;; 完成 1.2 后进入门禁阶段；失败门不推进且记录错误摘要。
      (let* ((bp (%load-bp %tmp1)) (bp1 (pi-blueprint-complete-task bp "1.2")))
        (pi-blueprint-save-state! (%state-file %tmp1) bp1)
      ) ;let*
      (%set-first-gate-command! %tmp1 "false")
      (let ((res (%run-pi %tmp1 base '("--print" "--plan-verify" "--timeout"
                                       "10"))))
        (check (car res) => 1)
        (check-true (string-contains? (cadr res) "FAIL"))
        (check-true (string-contains? (cadr res) "Phase not advanced"))
        (let ((gate (car (pi-phase-gates (pi-blueprint-find-phase (%load-bp %tmp1) "1")))))
          (check (pi-gate-passed? gate) => #f)
          (check-true (string? (pi-gate-error-message gate)))
        ) ;let
        (check (%phase-status %tmp1 "1") => "completed")
      ) ;let

      ;; 自动门通过但人工门未放行：不推进，退出码 1，但存在明确出口。
      (%set-first-gate-command! %tmp1 "true")
      (let ((res (%run-pi %tmp1 base '("--print" "--plan-verify" "--timeout"
                                       "10"))))
        (check (car res) => 1)
        (check-true (string-contains? (cadr res) "User approval still required"))
        (check (%phase-status %tmp1 "1") => "completed")
      ) ;let

      ;; --approve 放行人工门并推进阶段。
      (let ((res (%run-pi %tmp1 base '("--print" "--plan-verify" "--approve"
                                       "--timeout" "10"))
            ) ;res
           ) ;
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "Phase 1 verified."))
        (check (%phase-status %tmp1 "1") => "verified")
        (check (pi-blueprint-active-phase-id (%load-bp %tmp1)) => "2")
      ) ;let

      ;; 状态只写工程内 .pi/，不污染 ~/.pi 测试 fixture。
      (check (vector->list (path-list (path %fixture))) => fixture-before)
    ) ;lambda
    (lambda () (%cleanup!))
  ) ;dynamic-wind
) ;let*

(display "=== 验收点 8b: 无凭据 / 网络错误退出码 1 ===\n")
(check (%ok? "nocreds"
         "PI_CODING_AGENT_DIR=/nonexistent bin/gf pi --print 'hello' --no-session"
         60
       ) ;%ok?
  =>
  1
) ;check
(check (%ok? "nonet"
         "PI_CODING_AGENT_DIR=tools/pi/tests/fixtures/direct GF_PI_BASE_URL=http://127.0.0.1:1 GF_PI_MODEL=gemini-3.8-flash bin/gf pi --print 'hello' --no-session --timeout 5 --session-timeout 15"
         60
       ) ;%ok?
  =>
  1
) ;check

(display "=== 验收点 9: 构建 / fmt / check / doc 门禁 ===\n")
(check (%ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
(check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
(check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)

(display "=== 验收点 9b: 单文件行数 <= 300 ===\n")
(for-each
  (lambda (f)
    (check-true
      (<= (length (string-split (path-read-text f) "\n")) 300)
    ) ;check-true
  ) ;lambda
  '("tools/pi/liii/pi/blueprint.scm" "tools/pi/liii/pi/blueprint/args.scm"
    "tools/pi/liii/pi/blueprint/gate.scm" "tools/pi/liii/pi/blueprint/graph.scm"
    "tools/pi/liii/pi/blueprint/parse.scm"
    "tools/pi/liii/pi/blueprint/prompt.scm"
    "tools/pi/liii/pi/blueprint/render.scm"
    "tools/pi/liii/pi/blueprint/runtime.scm"
    "tools/pi/liii/pi/blueprint/state.scm"
    "tools/pi/liii/pi/blueprint/store.scm"
    "tools/pi/liii/pi/blueprint/tools.scm"
    "tools/pi/liii/pi/blueprint/transition.scm"
    "tools/pi/liii/pi/blueprint/types.scm"
    "tools/pi/liii/pi/blueprint/validate.scm"
    "tools/pi/liii/pi/modes/blueprint.scm"
    "tools/pi/liii/pi/modes/blueprint/common.scm"
    "tools/pi/liii/pi/modes/blueprint/generate.scm"
    "tools/pi/liii/pi/modes/blueprint/manage.scm"
    "tools/pi/liii/pi/modes/blueprint/verify.scm" "tools/pi/devel/047.scm")
) ;for-each

(display "=== 验收点 9c: 19 个新增单元测试 ===\n")
(for-each
  (lambda (f)
    (check (%ok? "unit" (string-append "timeout 120 bin/gf test " f) 150) => 0)
  ) ;lambda
  '("tools/pi/tests/liii/pi/blueprint/gate/pi-run-blueprint-gate-test.scm"
    "tools/pi/tests/liii/pi/blueprint/graph/pi-blueprint-detect-cycles-test.scm"
    "tools/pi/tests/liii/pi/blueprint/graph/pi-blueprint-ready-test.scm"
    "tools/pi/tests/liii/pi/blueprint/graph/pi-blueprint-topological-sort-test.scm"
    "tools/pi/tests/liii/pi/blueprint/parse/pi-extract-blueprint-json-test.scm"
    "tools/pi/tests/liii/pi/blueprint/parse/pi-parse-blueprint-plan-test.scm"
    "tools/pi/tests/liii/pi/blueprint/prompt/pi-build-blueprint-context-test.scm"
    "tools/pi/tests/liii/pi/blueprint/prompt/pi-build-blueprint-generate-prompt-test.scm"
    "tools/pi/tests/liii/pi/blueprint/render/pi-format-blueprint-progress-test.scm"
    "tools/pi/tests/liii/pi/blueprint/render/pi-render-blueprint-plan-test.scm"
    "tools/pi/tests/liii/pi/blueprint/state/pi-blueprint-recompute-blocked-test.scm"
    "tools/pi/tests/liii/pi/blueprint/state/pi-blueprint-start-task-test.scm"
    "tools/pi/tests/liii/pi/blueprint/store/pi-blueprint-load-state-test.scm"
    "tools/pi/tests/liii/pi/blueprint/store/pi-blueprint-save-state-test.scm"
    "tools/pi/tests/liii/pi/blueprint/tools/pi-execute-blueprint-update-test.scm"
    "tools/pi/tests/liii/pi/blueprint/transition/pi-blueprint-advance-phase-test.scm"
    "tools/pi/tests/liii/pi/blueprint/transition/pi-blueprint-complete-task-test.scm"
    "tools/pi/tests/liii/pi/blueprint/transition/pi-blueprint-set-gate-test.scm"
    "tools/pi/tests/liii/pi/blueprint/validate/pi-validate-blueprint-plan-test.scm")
) ;for-each

(display "=== 验收点 9d: CLI / JSON 回归与模式测试 ===\n")
(for-each
  (lambda (f)
    (check (%ok? "regress" (string-append "timeout 120 bin/gf test " f) 150) => 0)
  ) ;lambda
  '("tools/pi/tests/liii/pi/args/pi-parse-pi-args-blueprint-test.scm"
    "tools/pi/tests/liii/pi/modes/blueprint/pi-bp-mode-state-path-test.scm"
    "tools/pi/tests/liii/pi/modes/blueprint/pi-blueprint-extend-tool-names-test.scm"
    "tools/pi/tests/liii/pi/modes/blueprint/pi-blueprint-cli-e2e-test.scm"
    "tools/pi/tests/liii/pi-cli-e2e-test.scm"
    "tools/pi/tests/liii/pi-json-e2e-test.scm")
) ;for-each

(dynamic-wind (lambda () #f)
  (lambda () (%cleanup!) (display "=== 047 验收完成 ===\n"))
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
