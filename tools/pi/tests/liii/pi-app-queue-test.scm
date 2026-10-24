(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code tui)
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

  ;; 验证屏幕渲染内容中包含深蓝背景色码 (48;2;38;44;64) 与绿色成功色码 (48;2;35;55;35)
  (let* ((screen (pi-app-screen app))
         (lines (screen-lines screen))
         (full-text (string-join lines "\n"))
        ) ;
    (check (string-contains? full-text "48;2;38;44;64") => #t)
    (check (string-contains? full-text "测试用户指令") => #t)
    (check (string-contains? full-text "48;2;35;55;35") => #t)
    (check (string-contains? full-text "bash") => #t)
    (check (string-contains? full-text "Done!") => #t)
  ) ;let*
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: 异步/Busy 期间连续提交，消息在队列排队并在完成后自动消费
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

  ;; 2. 用户在长任务期间提交 2 条新消息
  (pi-app-submit! app "等待中的任务 1")
  (pi-app-submit! app "等待中的任务 2")

  ;; 此时任务在忙，handler 尚未处理这两条消息，它们都在队列中
  (check processed => '())
  (check (pi-queue-length (pi-app-queue app)) => 2)
  (check (pi-queue-items (pi-app-queue app))
    =>
    '("等待中的任务 1" "等待中的任务 2")
  ) ;check

  ;; 3. 前序长任务结束 (finish-task!) 并推进队列
  (pi-queue-finish-task! (pi-app-queue app))
  (pi-app-drain-queue! app)

  ;; 验证队列中的两条消息均被顺序自动消费处理完毕
  (check (reverse processed) => '("等待中的任务 1" "等待中的任务 2"))
  (check (pi-queue-empty? (pi-app-queue app)) => #t)
  (check (pi-queue-idle? (pi-app-queue app)) => #t)
) ;let*

(check-report)
