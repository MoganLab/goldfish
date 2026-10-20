(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi event)
  (liii pi message)
  (liii pi cost)
  (liii pi agent cost)
) ;import

(check-set-mode! 'report-failed)

;; [023] 模型定价表与成本统计 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(display "=== 验收点 1: 模型定价表内置与匹配查找 ===\n")
;; 1.1 内置主流模型：Gemini 3.8 Flash, Kimi k3, Claude 3.7 Sonnet
(let ((c (pi-model-cost "google" "gemini-3.8-flash" '())))
  (check (cdr (assoc 'input c)) => 0.75)
  (check (cdr (assoc 'output c)) => 3.75)
  (check (cdr (assoc 'cacheRead c)) => 0.075)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
) ;let

(let ((c (pi-model-cost "kimi-coding" "k3" '())))
  (check (cdr (assoc 'input c)) => 3.0)
  (check (cdr (assoc 'output c)) => 15.0)
  (check (cdr (assoc 'cacheRead c)) => 0.3)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
) ;let

(let ((c (pi-model-cost "anthropic" "claude-3-7-sonnet-20250219" '())))
  (check (cdr (assoc 'input c)) => 3.0)
  (check (cdr (assoc 'output c)) => 15.0)
  (check (cdr (assoc 'cacheRead c)) => 0.3)
  (check (cdr (assoc 'cacheWrite c)) => 3.75)
) ;let

;; 1.2 定价表缺失的模型兜底归零
(let ((c (pi-model-cost "custom-provider" "unknown-model" '())))
  (check (cdr (assoc 'input c)) => 0.0)
  (check (cdr (assoc 'output c)) => 0.0)
  (check (cdr (assoc 'cacheRead c)) => 0.0)
  (check (cdr (assoc 'cacheWrite c)) => 0.0)
) ;let

(display "=== 验收点 2: 单次 usage 的 cost 计算 ===\n")
;; 2.1 常规 usage
(let* ((cost-rates '((input . 1.0)
                     (output . 2.0)
                     (cacheRead . 0.1)
                     (cacheWrite . 0.5))
       ) ;cost-rates
       (usage '((input . 1000000)
                (output . 500000)
                (cacheRead . 1000000)
                (cacheWrite . 200000))
       ) ;usage
       (cost (pi-calculate-cost cost-rates usage))
      ) ;
  (check (cdr (assoc 'input cost)) => 1.0)
  (check (cdr (assoc 'output cost)) => 1.0)
  (check (cdr (assoc 'cacheRead cost)) => 0.1)
  (check (cdr (assoc 'cacheWrite cost)) => 0.1)
  (check (cdr (assoc 'total cost)) => 2.2)
) ;let*

;; 2.2 跨阶梯 tier 计算 (Gemini 2.5 Pro)
(let* ((cost-rates
         '((input . 1.25)
           (output . 10.0)
           (cacheRead . 0.125)
           (cacheWrite . 0.0)
           (tiers ((inputTokensAbove . 200000)
                   (input . 2.5)
                   (output . 15.0)
                   (cacheRead . 0.25)
                   (cacheWrite . 0.0))))
       ) ;cost-rates
       (usage '((input . 150000)
                (output . 10000)
                (cacheRead . 100000)
                (cacheWrite . 0))
       ) ;usage
       (cost (pi-calculate-cost cost-rates usage))
      ) ;
  (check (cdr (assoc 'input cost)) => 0.375)
  (check (cdr (assoc 'output cost)) => 0.15)
  (check (cdr (assoc 'cacheRead cost)) => 0.025)
  (check (cdr (assoc 'total cost)) => 0.55)
) ;let*

(display "=== 验收点 3: Prompt Cache 节省金额与会话累计 ===\n")
(let* ((cost-rates '((input . 3.0)
                     (output . 15.0)
                     (cacheRead . 0.3)
                     (cacheWrite . 0.0))
       ) ;cost-rates
       (usage '((input . 100000)
                (output . 5000)
                (cacheRead . 500000)
                (cacheWrite . 0))
       ) ;usage
       (savings (pi-calculate-cache-savings cost-rates usage))
      ) ;
  (check savings => 1.35)
) ;let*

(let* ((config '((provider . "google") (model-id . "gemini-3.8-flash")))
       (m1 (pi-assistant-message (list (pi-text-block "1"))
             :usage
             '((input . 1000000)
               (output . 100000)
               (cacheRead . 0)
               (cacheWrite . 0))
           ) ;pi-assistant-message
       ) ;m1
       (m2 (pi-assistant-message (list (pi-text-block "2"))
             :usage
             '((input . 500000)
               (output . 50000)
               (cacheRead . 500000)
               (cacheWrite . 0))
           ) ;pi-assistant-message
       ) ;m2
       (summary (pi-agent-session-cost-summary (list m1 m2) config))
      ) ;
  (check (cdr (assoc 'totalCost summary)) => 1.725)
  (check (cdr (assoc 'totalSavings summary)) => 0.3375)
) ;let*

(display "=== 验收点 4: 消息与事件结构带 cost 字段 ===\n")
;; 4.1 消息结构中的 usage 带 cost
(let* ((config '((provider . "google") (model-id . "gemini-3.8-flash")))
       (msg (pi-assistant-message (list (pi-text-block "hi"))
              :usage
              '((input . 1000000)
                (output . 100000)
                (cacheRead . 0)
                (cacheWrite . 0))
            ) ;pi-assistant-message
       ) ;msg
       (msg-with-cost (pi-agent-attach-cost msg config))
       (u (pi-assistant-message-usage msg-with-cost))
       (c (cdr (assoc 'cost u)))
      ) ;
  (check-true (pair? c))
  (check (cdr (assoc 'input c)) => 0.75)
  (check (cdr (assoc 'output c)) => 0.375)
  (check (cdr (assoc 'total c)) => 1.125)
) ;let*

;; 4.2 agent_end 事件结构带 cost 与 savings
(let* ((ev (pi-event-agent-end '() #f 1.725 0.3375)) (alist (pi-event->alist ev)))
  (check (pi-event-agent-end-cost ev) => 1.725)
  (check (pi-event-agent-end-savings ev) => 0.3375)
  (check (cdr (assoc "cost" alist)) => 1.725)
  (check (cdr (assoc "savings" alist)) => 0.3375)
) ;let*

(display "=== 验收点 5: TDD 独立单元测试全量通过 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/cost/pi-model-cost-test.scm" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/cost/pi-calculate-cost-test.scm"
         30
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/cost/pi-calculate-savings-test.scm"
         30
       ) ;%run-shell
  =>
  0
) ;check

(display "=== 验收点 6: 真实冒烟与期望输出 ===\n")
;; 6.1 print 模式真实冒烟
(let* ((tmp-out "tmp-023-smoke-print.txt")
       (ret (%run-shell (string-append "bin/gf pi --print \"hi\" > " tmp-out " 2>&1") 120)
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (> (string-length out) 0))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 6.2 JSON 模式真实冒烟
(let* ((tmp-out "tmp-023-smoke-json.txt")
       (ret (%run-shell (string-append "bin/gf pi --mode json \"hi\" > " tmp-out " 2>&1")
              120
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "\"agent_end\""))
  (check-true (string-contains? out "\"cost\":"))
  (check-true (string-contains? out "\"savings\":"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 7: 门禁检查通过 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
(check (%run-shell "git diff --exit-code tools/pi/liii/ tools/pi/tests/" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)
