;; demo/tui/interactive_card_demo.scm - 用于 tmux 集成测试的 TUI 交互式卡片演示
(set! *load-path* (cons "tools/pi" *load-path*))

(import (scheme base) (liii pi tui) (liii pi tui handler))

(define mock-runner
  (lambda (parsed cfg initial-msg emit on-finish)
    ;; 1. 模拟 bash 工具多行调用与结果
    (emit 'tool_execution_start "c1" "bash" '(("command" . "git status -s")))
    (emit 'tool_execution_end
      "c1"
      "bash"
      " M tools/pi/liii/pi/tui/renderers.scm\n?? tools/pi/tests/liii/pi/tui/renderers-test.scm"
      #f
    ) ;emit

    ;; 2. 模拟 read 工具调用与结果
    (emit 'tool_execution_start
      "c2"
      "read"
      '(("file_path" . "CLAUDE.md") ("offset" . 1) ("limit" . 10))
    ) ;emit
    (emit 'tool_execution_end
      "c2"
      "read"
      "# Claude Code 工作规则\n## 任务执行规则"
      #f
    ) ;emit

    ;; 3. 模拟 assistant 回复
    (emit 'message_end
      '(("role" . "assistant")
        ("content"
         . #((("type" . "text")
              ("text" . "工具调用与分析已全部完成。"))
           ) ;#
        ))
    ) ;emit
    (emit 'agent_settled #f)
  ) ;lambda
) ;define

(define mock-cfg '((provider . "mock") (api-key . "mock-key")))

(define app
  (make-pi-app :handler (make-tui-agent-handler '() mock-cfg :runner mock-runner))
) ;define
(pi-app-run app)
