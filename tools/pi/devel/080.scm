(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii sys)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii pi prompt)
  (liii pi args)
  (liii pi agent)
  (liii pi agent budget)
  (liii pi modes flow)
) ;import

(check-set-mode! 'report-failed)

;; [080] 验收脚本：会话时限注入 system prompt：让 agent 感知剩余预算

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-080-" tag ".txt")))
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
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 080 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/agent.scm" "tools/pi/liii/pi/prompt.scm"
    "tools/pi/liii/pi/modes/flow.scm" "tools/pi/liii/pi/prompt/context.scm"
    "tools/pi/liii/pi/agent/budget.scm" "tools/pi/liii/pi/agent/loop.scm"
    "tools/pi/tests/liii/pi/pi-build-system-prompt-test.scm"
    "tools/pi/tests/liii/pi/agent/pi-resolve-turn-system-prompt-test.scm"
    "tools/pi/devel/080.scm" "tools/pi/devel/080.md")
) ;for-each

(display "=== 验收点 3: 静态系统提示词注入预算约束说明 (<session_budget>) ===\n"
) ;display
;; 3.1 默认预算参数（600s, 200 轮）生成完整规范节点
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '())))
  (check-true (string-contains? sp "<session_budget>"))
  (check-true (string-contains? sp
                "- Session Timeout: 600 seconds (hard wall-clock deadline)"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp "- Max Turns: 200 turns."))
  (check-true (string-contains? sp "- Pacing: Pace your work accordingly."))
  (check-true (string-contains? sp "</session_budget>"))
) ;let

;; 3.2 自定义有限预算（例如 1800s, 50 轮）
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '() '() 1800 50))
     ) ;
  (check-true (string-contains? sp "- Session Timeout: 1800 seconds"))
  (check-true (string-contains? sp "- Max Turns: 50 turns."))
) ;let

;; 3.3 禁用超时与无限轮次（0 或 #f）正确体现 None / Unlimited
(let ((sp1 (pi-build-system-prompt '("read") "/test/cwd" #f #f '() '() 0 0))
      (sp2 (pi-build-system-prompt '("read") "/test/cwd" #f #f '() '() #f #f))
     ) ;
  (check-true (string-contains? sp1
                "- Session Timeout: None (no wall-clock deadline configured)."
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp1 "- Max Turns: Unlimited (no turn limit configured).")
  ) ;check-true
  (check-true (string-contains? sp2
                "- Session Timeout: None (no wall-clock deadline configured)."
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp2 "- Max Turns: Unlimited (no turn limit configured).")
  ) ;check-true
) ;let

(display "=== 验收点 4: 回合级动态预算与进度可见性 (<current_turn_budget>) ===\n"
) ;display
;; 4.1 基础回退兼容性：字符串原样返回，不影响既有行为
(check (pi-resolve-turn-system-prompt "base prompt" '()) => "base prompt")

;; 4.2 充裕预算回合：包含当前轮次与剩余单调时间，无预警
(let ((sp (pi-resolve-turn-system-prompt "base" '() 1 200 500.0 500.0 600)))
  (check-true (string-contains? sp "<current_turn_budget>"))
  (check-true (string-contains? sp "Turn Status: Turn 1 (out of max 200, 199 remaining).")
  ) ;check-true
  (check-true (string-contains? sp
                "Time Status: 500 seconds remaining before session timeout."
              ) ;string-contains?
  ) ;check-true
  (check-false (string-contains? sp "<budget_warning>"))
) ;let

;; 4.3 无限轮次与无时间死线
(let ((sp (pi-resolve-turn-system-prompt "base" '() 10 0 #f #f #f)))
  (check-true (string-contains? sp "Turn Status: Turn 10 (unlimited turns)."))
  (check-true (string-contains? sp "Time Status: Unlimited (no session timeout).")
  ) ;check-true
  (check-false (string-contains? sp "<budget_warning>"))
) ;let

(display "=== 验收点 5: 临界预算警报与自动收敛引导 (<budget_warning>) ===\n"
) ;display
;; 5.1 剩余轮次临界预警 (剩余 <= 3 轮)
(let ((sp (pi-resolve-turn-system-prompt "base" '() 198 200 400.0 400.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "WARNING: Approaching maximum turn limit (<= 3 turns left)."
              ) ;string-contains?
  ) ;check-true
) ;let

;; 5.2 剩余时间临界预警 (剩余时间不足 60s 或 < 10%)
(let ((sp (pi-resolve-turn-system-prompt "base" '() 5 200 45.0 45.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "WARNING: Approaching session timeout deadline (< 60s or < 10% time left)."
              ) ;string-contains?
  ) ;check-true
) ;let

;; 5.3 轮次与时间双重危机预警
(let ((sp (pi-resolve-turn-system-prompt "base" '() 199 200 30.0 30.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "CRITICAL BUDGET WARNING: You are nearly out of turns AND time!"
              ) ;string-contains?
  ) ;check-true
) ;let

(display "=== 验收点 6: CLI 与执行流程无缝串联 (flow.scm 贯通) ===\n"
) ;display
;; 6.1 默认 CLI 参数穿透到系统提示词
(let* ((parsed (parse-pi-args '()))
       (cfg '((tools "read" "bash")))
       (sp (prepare-system-prompt cfg parsed))
      ) ;
  (check-true (string-contains? sp "- Session Timeout: 600 seconds"))
  (check-true (string-contains? sp "- Max Turns: 200 turns."))
) ;let*

;; 6.2 显式 CLI 参数 (--session-timeout 5400 --max-turns 0) 准确贯通
(let* ((parsed (parse-pi-args '("--session-timeout" "5400" "--max-turns" "0")))
       (cfg '((tools "read" "bash")))
       (sp (prepare-system-prompt cfg parsed))
      ) ;
  (check-true (string-contains? sp "- Session Timeout: 5400 seconds"))
  (check-true (string-contains? sp "- Max Turns: Unlimited (no turn limit configured).")
  ) ;check-true
) ;let*

;; 6.3 真实命令行冒烟验证：--session-timeout 负数立即截断报错
(let ((tmp "tmp-080-st-neg.txt"))
  (let ((ret (run (list (pi-find-bash-path)
                    "-c"
                    "bin/gf pi -p --session-timeout -1 'test' > tmp-080-st-neg.txt 2>&1"
                  ) ;list
               :stdin   'null
               :timeout 30
             ) ;run
        ) ;ret
        (out (path-read-text (path tmp)))
       ) ;
    (when (file-exists? tmp)
      (delete-file tmp)
    ) ;when
    (check ret => 1)
    (check-true (string-contains? out "Session timeout exceeded"))
  ) ;let
) ;let

;; 6.4 真实命令行冒烟验证：--help 包含 session-timeout 与 max-turns 描述
(let ((tmp "tmp-080-help.txt"))
  (let ((ret (run (list (pi-find-bash-path) "-c" "bin/gf pi --help > tmp-080-help.txt 2>&1")
               :stdin   'null
               :timeout 30
             ) ;run
        ) ;ret
        (out (path-read-text (path tmp)))
       ) ;
    (when (file-exists? tmp)
      (delete-file tmp)
    ) ;when
    (check ret => 0)
    (check-true (string-contains? out "--max-turns"))
    (check-true (string-contains? out "--session-timeout"))
  ) ;let
) ;let

(display "=== 验收点 7: 全量单元测试回归 ===\n")
(check (%run-ok? "t-prompt"
         "bin/gf test tools/pi/tests/liii/pi/pi-build-system-prompt-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-agent-sp"
         "bin/gf test tools/pi/tests/liii/pi/agent/pi-resolve-turn-system-prompt-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "all-tests" "bin/gf test tools/pi/tests/liii/" 300) => 0)

(check-report)
