(import (scheme base)
  (liii check)
  (liii string)
  (liii go)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

(define (recv-ev! ch)
  (chan-try-recv! ch 'none)
) ;define

(define (stream-event type . fields)
  (cons (cons 'type type) fields)
) ;define

;; 未凑齐完整行时不上屏；后续增量拼出完整行后立即投递。
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update '() (stream-event "thinking_delta" '(delta . "第一")))
  (check (recv-ev! ch) => 'none)
  (emit 'message_update
    '()
    (stream-event "thinking_delta" '(delta . "行\n第二"))
  ) ;emit
  (check (recv-ev! ch) => '(assistant_thinking_line "第一行"))
  (check (recv-ev! ch) => 'none)
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_line "第二"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (check (recv-ev! ch) => 'none)
) ;let*

;; 单个增量含多行时逐行投递，thinking_end 冲出残余半行。
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update
    '()
    (stream-event "thinking_delta" '(delta . "line 1\nline 2\nline 3"))
  ) ;emit
  (check (recv-ev! ch) => '(assistant_thinking_line "line 1"))
  (check (recv-ev! ch) => '(assistant_thinking_line "line 2"))
  (check (recv-ev! ch) => 'none)
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_line "line 3"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (check (recv-ev! ch) => 'none)
) ;let*

;; 每个 thinking 块独立冲尾；全空白残余行不产生空卡片。
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update
    '()
    (stream-event "thinking_delta" '(delta . "块一尾行"))
  ) ;emit
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_line "块一尾行"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (emit 'message_update
    '()
    (stream-event "thinking_delta" '(delta . "块二\n  \n"))
  ) ;emit
  (check (recv-ev! ch) => '(assistant_thinking_line "块二"))
  (check (recv-ev! ch) => '(assistant_thinking_line "  "))
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (check (recv-ev! ch) => 'none)
) ;let*

;; thinking_end 无论有无残余行都投递块结束事件，连续多个块每块一次，
;; 供 UI 关闭当前卡片（每个 thinking 块一张卡片）。
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update '() (stream-event "thinking_delta" '(delta . "块一\n")))
  (check (recv-ev! ch) => '(assistant_thinking_line "块一"))
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (emit 'message_update
    '()
    (stream-event "thinking_delta" '(delta . "块二\n"))
  ) ;emit
  (check (recv-ev! ch) => '(assistant_thinking_line "块二"))
  (emit 'message_update '() (stream-event "thinking_end"))
  (check (recv-ev! ch) => '(assistant_thinking_end))
  (check (recv-ev! ch) => 'none)
) ;let*

;; 同一 message_update 通道中的正文、工具增量与 thinking_start 均不触发 thinking 行事件。
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'message_update '() (stream-event "thinking_start"))
  (emit 'message_update '() (stream-event "text_delta" '(delta . "正文\n")))
  (emit 'message_update '() (stream-event "toolcall_delta" '(delta . "参数\n")))
  (check (recv-ev! ch) => 'none)
) ;let*

;; agent loop 实际传入 usage 与 provider 事件两个参数；worker 只读取后者。
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (usage '((input . 10) (output . 2)))
      ) ;
  (emit 'message_update
    usage
    (stream-event "thinking_delta" '(delta . "usage 不影响切行\n"))
  ) ;emit
  (check (recv-ev! ch) => '(assistant_thinking_line "usage 不影响切行"))
  (check (recv-ev! ch) => 'none)
) ;let*

(check-report)
