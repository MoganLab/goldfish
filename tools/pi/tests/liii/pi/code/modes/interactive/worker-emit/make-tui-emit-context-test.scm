;; make-tui-emit
;; worker 事件桥的上下文占用事件：message_usage 事件携带上下文窗口，
;; compaction 事件通知前端上下文已重建（占比转为未知）
;;
;; 语法
;; ----
;; (make-tui-emit event-chan context-window)
;;
;; 参数
;; ----
;; event-chan : channel
;; 事件投递通道。
;; context-window : number | #f (可选)
;; 当前模型的上下文窗口 Token 数，缺省为 #f。
;;
;; 返回值
;; ----
;; procedure
;; 事件翻译回调。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

(define (recv-ev! ch)
  (chan-try-recv! ch 'none)
) ;define

;; 1. message_usage 事件携带上下文窗口
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch 1048576))
       (usage '((input . 100) (output . 50)))
       (msg '((role . "assistant")
              (content . "hi")
              (usage (input . 100) (output . 50)))
       ) ;msg
      ) ;
  (emit 'message_end msg)
  (check (recv-ev! ch) => '(message_usage ((input . 100) (output . 50)) 1048576))
  (check (recv-ev! ch) => 'none)
) ;let*

;; 2. 未提供上下文窗口时事件负载为 #f
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch))
       (msg '((role . "assistant") (content . "hi") (usage (input . 1))))
      ) ;
  (emit 'message_end msg)
  (check (recv-ev! ch) => '(message_usage ((input . 1)) #f))
) ;let*

;; 3. all-in-one 实际节点切换后，usage 使用该节点的窗口而不是伪模型窗口
(let* ((ch (make-chan 16))
       (emit (make-tui-emit ch 128000))
       (msg '((role . "assistant") (content . "hi") (usage (input . 1))))
      ) ;
  (emit 'active_node '("zai-coding-cn" "glm-5.3" "high" 204800))
  (check (recv-ev! ch)
    =>
    '(active_node ("zai-coding-cn" "glm-5.3" "high" 204800))
  ) ;check
  (emit 'message_end msg)
  (check (recv-ev! ch) => '(message_usage ((input . 1)) 204800))
) ;let*

;; 4. compaction 事件透传给前端
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch 1048576)))
  (emit 'compaction)
  (check (recv-ev! ch) => '(compaction))
  (check (recv-ev! ch) => 'none)
) ;let*

(check-report)
