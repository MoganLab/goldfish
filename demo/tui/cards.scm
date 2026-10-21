;; demo/tui/cards.scm - pi 交互卡片视觉效果展示
(set! *load-path* (cons "tools/pi" *load-path*))

(import (scheme base) (liii tui) (liii pi tui views))

(define (show-card card width)
  (let ((lines (component-render card width)))
    (for-each (lambda (l) (display l) (newline)) lines)
  ) ;let
) ;define

(define w 60)

(display "============================================================\n")
(display "     pi TUI 核心背景卡片视觉展示 (宽度 = 60)\n")
(display "============================================================\n\n")

(display "1. 用户提问卡片 (深蓝背景 userMessageBg):\n")
(show-card (make-user-card "请帮我运行测试并检查代码格式") w)

(display "2. 工具调用等待中卡片 (暗灰背景 toolPendingBg):\n")
(show-card (make-tool-card "bash" "xmake b goldfish" 'pending) w)

(display "3. 工具调用成功卡片 (标志性【深绿背景】 toolSuccessBg):\n"
) ;display
(show-card (make-tool-card "bash"
             "bin/gf test ...-test.scm"
             'success
             "37 correct, 0 failed. PASS"
           ) ;make-tool-card
  w
) ;show-card

(display "4. 工具调用出错卡片 (警示【深红背景】 toolErrorBg):\n"
) ;display
(show-card (make-tool-card "read"
             "non_existent.scm"
             'error
             "File not found: non_existent.scm"
           ) ;make-tool-card
  w
) ;show-card

(display "5. 助手回复文本 (透明底色):\n")
(show-card (make-assistant-card "所有单元测试已全部通过，代码格式符合规范。"
           ) ;make-assistant-card
  w
) ;show-card

(display "6. 等待队列提示卡片 (暗淡前景色):\n")
(show-card (make-queued-card "等待前序任务完成后执行下一条指令")
  w
) ;show-card

(display "============================================================\n")
(display "展示完成！\n")
