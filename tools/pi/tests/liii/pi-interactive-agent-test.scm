(import (scheme base)
        (scheme process-context)
        (liii check)
        (liii tui)
        (liii pi tui)
        (liii pi tui handler)
        (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 测试 make-tui-agent-handler 构造与基础类型
(let* ((mock-cfg '((provider . "mock") (api-key . "mock-key")))
       (mock-runner (lambda (parsed cfg initial-msg emit on-finish)
                      ;; 模拟执行：先派发工具调用，再派发助手回复，最后通知结算
                      (emit 'tool_execution_end "call_1" "bash" "test result" #f)
                      (emit 'message_end '(("role" . "assistant")
                                          ("content" . #((("type" . "text") ("text" . "任务完成"))))))
                      (emit 'agent_settled #f)))
       (handler (make-tui-agent-handler '() mock-cfg :runner mock-runner))
       (app (make-pi-app :terminal (make-virtual-terminal 80 24) :handler handler)))
  (check (procedure? handler) => #t)
  
  ;; 2. 模拟用户输入提交，验证卡片生成与队列结算状态
  (pi-app-submit! app "请运行单元测试")
  
  ;; 验证 chat 历史中已正确追加了：
  ;; 1) 用户深蓝卡片
  ;; 2) 工具深绿成功卡片
  ;; 3) 助手回复文本卡片
  (let ((chat (pi-app-chat app)))
    (check (length chat) => 3)
    (check (pi-queue-idle? (pi-app-queue app)) => #t))
  
  ;; 3. 模拟工具出错分支
  (let* ((error-runner (lambda (parsed cfg initial-msg emit on-finish)
                         (emit 'tool_execution_end "call_2" "read" "File not found" #t)
                         (emit 'agent_settled #f)))
         (err-handler (make-tui-agent-handler '() mock-cfg :runner error-runner))
         (app2 (make-pi-app :terminal (make-virtual-terminal 80 24) :handler err-handler)))
    (pi-app-submit! app2 "读取不存在的文件")
    (let ((chat (pi-app-chat app2)))
      (check (length chat) => 2)
      (check (pi-queue-idle? (pi-app-queue app2)) => #t))))

;; 4. 验证缺少 API Key 时不崩溃且向用户展示友好提示卡片
(let* ((empty-cfg '((provider . "anthropic") (api-key . #f)))
       (handler (make-tui-agent-handler '() empty-cfg))
       (app (make-pi-app :terminal (make-virtual-terminal 80 24) :handler handler)))
  (pi-app-submit! app "测试无凭证")
  ;; 应当有 2 个条目：1) 用户发送的消息卡片 2) 凭证缺失的报错卡片
  (let ((chat (pi-app-chat app)))
    (check (length chat) => 2)
    (check (pi-queue-idle? (pi-app-queue app)) => #t)))

;; 5. 验证 [pi retry] 重试事件作为卡片输出而非污染终端
(let* ((mock-cfg '((provider . "mock") (api-key . "mock-key")))
       (retry-runner (lambda (parsed cfg initial-msg emit on-finish)
                       (emit 'retry "[pi retry] HTTP 429, attempt 1/12, waiting 2.0s")
                       (emit 'agent_settled #f)))
       (handler (make-tui-agent-handler '() mock-cfg :runner retry-runner))
       (app (make-pi-app :terminal (make-virtual-terminal 80 24) :handler handler)))
  (pi-app-submit! app "触发429重试")
  (let ((chat (pi-app-chat app)))
    ;; 应当有 2 个条目：1) 用户深蓝卡片 2) ↻ 重试卡片
    (check (length chat) => 2)
    (check (pi-queue-idle? (pi-app-queue app)) => #t)))

(check-report)
