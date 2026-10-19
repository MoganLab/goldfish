(import (liii check)
  (scheme file)
  (scheme process-context)
  (liii string)
  (liii list)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi blueprint types)
  (liii pi blueprint graph)
  (liii pi blueprint state)
  (liii pi blueprint transition)
  (liii pi blueprint store)
) ;import

(check-set-mode! 'report-failed)

;; [047] blueprint 命令行端到端：生成 / 状态 / 下一任务 / 验证门 / 放弃，
;; 以及 print 与 JSON 的输出分流。辅助函数见 pi-blueprint-e2e-support.scm。

(define %support-rel
  "tests/liii/pi/modes/blueprint/pi-blueprint-e2e-support.scm"
) ;define

(define %support-path
  (let ((a
          (path->string
            (path-join (path (path->string (path-cwd))) "tools/pi" %support-rel)
          ) ;path->string
        ) ;a
       ) ;
    (if (path-exists? (path a))
      a
      (path->string
        (path-join (path (path->string (path-cwd))) %support-rel)
      ) ;path->string
    ) ;if
  ) ;let
) ;define
(load %support-path)

(define (run-local-checks! env)
  (display "=== 无活动计划的四条本地命令 ===\n")
  (for-each
    (lambda (flag)
      (let ((res (run-pi %tmp2 env (list "--print" flag))))
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "No active blueprint."))
      ) ;let
    ) ;lambda
    '("--plan-status" "--plan-next" "--plan-verify" "--plan-abandon")
  ) ;for-each
) ;define

(display "=== blueprint 命令行端到端 ===\n")
(setup-dir! %tmp1)
(setup-dir! %tmp2)
(let* ((stub (start-stub!))
       (base (string-append "http://127.0.0.1:" (number->string (cdr stub))))
       (env (e2e-env base))
       (env-plan (e2e-env (string-append base "/blueprint-plan")))
       (env-next (e2e-env (string-append base "/blueprint-next")))
       (fixture-before (snapshot-fixture))
      ) ;
  (dynamic-wind (lambda () #f)
    (lambda ()
      (run-local-checks! env)

      ;; 1) JSON 模式生成：stdout 纯 JSONL，计划写 stderr。
      (let* ((res (run-pi %tmp1
                    env-plan
                    '("--mode" "json" "--blueprint" "Add two numbers"
                      "--no-session" "--timeout" "30" "--session-timeout" "120")
                  ) ;run-pi
             ) ;res
             (code (car res))
             (out (cadr res))
             (err (caddr res))
            ) ;
        (check code => 0)
        (check-pure-jsonl out)
        (check-true (string-contains? err "# Blueprint: Add two numbers"))
        (check-false (string-contains? out "# Blueprint"))
        (check-true (path-exists? (path (state-file %tmp1))))
        (check-true
          (path-exists? (path (pi-blueprint-plan-path (state-file %tmp1))))
        ) ;check-true
        (check-true
          (path-exists? (path (pi-blueprint-history-path (state-file %tmp1))))
        ) ;check-true
      ) ;let*

      ;; 2) 放弃后可用 print 模式重新生成。
      (let ((res (run-pi %tmp1 env '("--print" "--plan-abandon"))))
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "Blueprint abandoned."))
        (check (pi-blueprint-status (load-bp %tmp1)) => "abandoned")
        (check (pi-blueprint-active-phase-id (load-bp %tmp1)) => #f)
      ) ;let
      (let ((res (run-pi %tmp1
                   env-plan
                   '("--print" "--blueprint" "Add two numbers" "--no-session"
                     "--timeout" "30" "--session-timeout" "120")
                 ) ;run-pi
            ) ;res
           ) ;
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "# Blueprint: Add two numbers"))
        (check-true (string-contains? (cadr res) "Progress: 0/4 tasks (0%)"))
        (check-true (string-contains? (cadr res) "## Phase 1: Foundations (active)"))
      ) ;let

      ;; 3) plan-status 输出进度与计划。
      (let ((res (run-pi %tmp1 env '("--print" "--plan-status"))))
        (check (car res) => 0)
        (check-true (string-contains? (cadr res) "Progress: 0/4 tasks (0%)"))
        (check-true (string-contains? (cadr res) "# Blueprint: Add two numbers"))
      ) ;let

      ;; 4) plan-next：模型调用 blueprint_update 完成 1.1。
      (let* ((res (run-pi %tmp1
                    env-next
                    '("--print" "--plan-next" "--no-session" "--timeout" "30"
                      "--session-timeout" "120")
                  ) ;run-pi
             ) ;res
             (out (cadr res))
            ) ;
        (check (car res) => 0)
        (check-true (string-contains? out "## Next Task: 1.1 - Add helper"))
        (check-true (string-contains? out "Task done."))
        (check-true (string-contains? out "Progress: 1/4 tasks (25%)"))
        (check
          (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks (load-bp %tmp1)) "1.1")
          ) ;pi-task-status
          =>
          "completed"
        ) ;check
      ) ;let*

      ;; 5) 门禁失败：custom_command 返回 false。
      (complete-phase-one! %tmp1)
      (set-phase-gate-command! %tmp1 "1" 0 "false")
      (let* ((res (run-pi %tmp1 env '("--print" "--plan-verify" "--timeout" "10")))
             (out (cadr res))
            ) ;
        (check (car res) => 1)
        (check-true (string-contains? out "FAIL"))
        (check-true (string-contains? out "Phase not advanced"))
        (let ((gate (car (pi-phase-gates (pi-blueprint-find-phase (load-bp %tmp1) "1")))))
          (check (pi-gate-passed? gate) => #f)
          (check-true (string? (pi-gate-error-message gate)))
        ) ;let
        (check (phase-status %tmp1 "1") => "completed")
      ) ;let*

      ;; 6) 自动门通过但人工门未放行 → 不推进，退出码 1。
      (set-phase-gate-command! %tmp1 "1" 0 "true")
      (let* ((res (run-pi %tmp1 env '("--print" "--plan-verify" "--timeout" "10")))
             (out (cadr res))
            ) ;
        (check (car res) => 1)
        (check-true (string-contains? out "User approval still required"))
        (check (phase-status %tmp1 "1") => "completed")
      ) ;let*

      ;; 7) --approve 放行人工门并推进阶段。
      (let* ((res (run-pi %tmp1 env '("--print" "--plan-verify" "--approve"
                                      "--timeout" "10"))
             ) ;res
             (out (cadr res))
            ) ;
        (check (car res) => 0)
        (check-true (string-contains? out "Phase 1 verified."))
        (check (phase-status %tmp1 "1") => "verified")
        (check (pi-blueprint-active-phase-id (load-bp %tmp1)) => "2")
      ) ;let*
      (check (snapshot-fixture) => fixture-before)
    ) ;lambda
    (lambda ()
      (process-kill (car stub))
      (process-close (car stub))
      (cleanup-dir! %tmp1)
      (cleanup-dir! %tmp2)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
