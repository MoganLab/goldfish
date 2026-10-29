(import (scheme base)
  (liii go)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive)
  (liii pi agent queue)
) ;import

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 测试项 1: app 初始化时具备空等待队列与卡片容器
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (check (pi-app? app) => #t)
  (check (pi-queue-empty? (pi-app-queue app)) => #t)
  (check (pi-queue-idle? (pi-app-queue app)) => #t)
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: 用户提交消息后先入队列并触发调度上屏深蓝卡片
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (handled-msgs '())
       (app
         (make-pi-app
           :terminal vterm
           :handler
           (lambda (app-inst msg)
             (set! handled-msgs (cons msg handled-msgs))
             ;; 模拟工具调用成功上屏绿色卡片
             (pi-app-add-tool-result! app-inst "bash" "echo hi" 'success "hi")
             ;; 模拟助手回复上屏
             (pi-app-add-assistant-text! app-inst "Done!")
           ) ;lambda
         ) ;make-pi-app
       ) ;app
      ) ;

  ;; 模拟用户在 editor 提交消息
  (pi-app-submit! app "测试用户指令")

  ;; 验证 handler 成功被调度消费
  (check handled-msgs => '("测试用户指令"))
  ;; 验证任务完成后恢复为 idle
  (check (pi-queue-idle? (pi-app-queue app)) => #t)

  ;; 验证屏幕渲染内容中包含用户卡片背景色码 (48;2;33;59;73) 与绿色成功色码 (48;2;37;65;49)
  (let* ((screen (pi-app-screen app))
         (lines (alt-screen-lines screen))
         (full-text (string-join lines "\n"))
        ) ;
    (check (string-contains? full-text "48;2;33;59;73") => #t)
    (check (string-contains? full-text "测试用户指令") => #t)
    (check (string-contains? full-text "48;2;37;65;49") => #t)
    (check (string-contains? full-text "bash") => #t)
    (check (string-contains? full-text "Done!") => #t)
  ) ;let*
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: Busy 期间回车提交 → steering 注入（通道 + 显示列表），
;; 等待队列（items）不再接收；消费事件上屏用户卡片并同步显示列表
;; -----------------------------------------------------------------------------
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (processed '())
       (app
         (make-pi-app
           :terminal vterm
           :handler
           (lambda (app-inst msg) (set! processed (cons msg processed)))
         ) ;make-pi-app
       ) ;app
      ) ;

  ;; 1. 手动将状态设为 busy，模拟正在执行长任务
  (pi-queue-set-busy! (pi-app-queue app))

  ;; 2. 用户在长任务期间回车提交 2 条新消息：进入 steering 注入路径
  (pi-app-submit! app "运行中插话 1")
  (pi-app-submit! app "运行中插话 2")

  ;; 等待队列（items）不再接收；steering 显示列表与通道各就各位
  (check processed => '())
  (check (pi-queue-length (pi-app-queue app)) => 0)
  (check (%pi-app-steering-pending app) => '("运行中插话 1" "运行中插话 2"))
  (check (chan-try-recv! (pi-app-steering-chan app) #f) => "运行中插话 1")
  (check (chan-try-recv! (pi-app-steering-chan app) #f) => "运行中插话 2")

  ;; 3. 模拟 worker 消费第一条：回发 user_message 事件后主线程同步显示
  (chan-send! (pi-app-event-chan app) '(user_message "运行中插话 1") 10)
  (pi-app-poll-events! app)
  (check (%pi-app-steering-pending app) => '("运行中插话 2"))
  ;; 用户卡片已上屏（深蓝背景色码）
  (let* ((screen (pi-app-screen app))
         (full-text (string-join (alt-screen-lines screen) "\n"))
        ) ;let*
    (check (string-contains? full-text "运行中插话 1") => #t)
    (check (string-contains? full-text "Steering: 运行中插话 2") => #t)
  ) ;let*

  ;; 4. busy 期间 alt+enter（follow-up 模式）：走 follow-up 通道而非 steering
  (pi-app-submit! app "结束后再做" 'follow-up)
  (check (%pi-app-steering-pending app) => '("运行中插话 2"))
  (check (%pi-app-follow-up-pending app) => '("结束后再做"))
  (check (chan-try-recv! (pi-app-follow-up-chan app) #f) => "结束后再做")

  ;; 5. ESC 撤回：steering → follow-up 合并返回并清空显示
  (check (pi-app-restore-queues! app) => '("运行中插话 2" "结束后再做"))
  (check (%pi-app-steering-pending app) => '())
  (check (%pi-app-follow-up-pending app) => '())
) ;let*

(check-report)
