(import (scheme base)
  (liii check)
  (liii go)
  (liii string)
  (liii list)
  (liii tui)
  (liii pi code modes interactive bash)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; Ctrl+T 切换思考块显示/隐藏（对齐上游 app.thinking.toggle）：
;; 1. 初始状态来自 make-pi-app :hide-thinking（由 settings.json 恢复）
;; 2. 切换后 chat 中已有 thinking 卡片即时折叠/展开为单行 Thinking...
;; 3. 切换不产生任何状态提示卡片（Thinking blocks: ... 已移除）
;; 4. 切换触发 on-save-hide-thinking 持久化回调
;; 5. 切换后新建的 thinking 卡片继承当前状态
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf
               :detect-size-fn (lambda () '(80 24))))
       (saved '())
       (app (make-pi-app :terminal term
              :handler (lambda (a m) #t)
              :hide-thinking #t
              :on-save-hide-thinking (lambda (hide) (set! saved (cons hide saved)))
            ) ;make-pi-app
           )
       (screen (pi-app-screen app))
      ) ;
  (check (pi-app-hide-thinking? app) => #t)

  ;; 流入两行思考内容：隐藏态下折叠为单行 Thinking...
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_line "推理甲") 1000)
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_line "推理乙") 1000)
  (pi-app-poll-events! app)
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-true (string-contains? text "Thinking..."))
    (check-false (string-contains? text "推理甲"))
    (check-false (string-contains? text "推理乙"))
  ) ;let

  ;; 第一次 Ctrl+T：显示思考内容，回调收到 #f；不产生状态提示
  (pi-app-toggle-thinking! app)
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-true (string-contains? text "推理甲"))
    (check-true (string-contains? text "推理乙"))
    (check-false (string-contains? text "Thinking blocks:"))
    (check-false (string-contains? text "Thinking..."))
  ) ;let
  (check (car saved) => #f)

  ;; 第二次 Ctrl+T：重新折叠，回调收到 #t；仍无状态提示
  (pi-app-toggle-thinking! app)
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-true (string-contains? text "Thinking..."))
    (check-false (string-contains? text "推理甲"))
    (check-false (string-contains? text "Thinking blocks:"))
  ) ;let
  (check saved => '(#t #f))

  ;; 折叠状态下新建卡片继承隐藏状态：thinking_end 结算旧卡后再来新思考行
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_end) 1000)
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_line "新思考") 1000)
  (pi-app-poll-events! app)
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-false (string-contains? text "新思考"))
    (check-true (string-contains? text "Thinking..."))
  ) ;let

  ;; 显示状态下新建卡片同样继承：切换到显示后再来新思考行
  (pi-app-toggle-thinking! app)
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_end) 1000)
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_line "再思考") 1000)
  (pi-app-poll-events! app)
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-true (string-contains? text "再思考"))
  ) ;let
) ;let*

;; 默认显示：不带 :hide-thinking 构造的 app 思考内容直接可见
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf
               :detect-size-fn (lambda () '(80 24))))
       (app (make-pi-app :terminal term :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
      ) ;
  (check (pi-app-hide-thinking? app) => #f)
  (chan-send! (pi-app-event-chan app) '(assistant_thinking_line "直出思考") 1000)
  (pi-app-poll-events! app)
  (pi-app-render! app)
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-true (string-contains? text "直出思考"))
  ) ;let

  ;; 无 on-save 回调时切换不报错，仅改状态
  (pi-app-toggle-thinking! app)
  (check (pi-app-hide-thinking? app) => #t)
) ;let*

;; 无状态提示：多次切换、中间插入新卡片后再切换，
;; chat 中始终不出现任何状态卡片（Thinking blocks: ... 提示已移除）
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf
               :detect-size-fn (lambda () '(80 24))))
       (app (make-pi-app :terminal term :handler (lambda (a m) #t)))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-toggle-thinking! app)
  (pi-app-toggle-thinking! app)
  (container-add! (%pi-app-chat app) (make-bash-card "ls"))
  (pi-app-toggle-thinking! app)
  (pi-app-render! app)
  (check
    (length (filter status-card? (container-children (%pi-app-chat app))))
    => 0
  ) ;check
  (let ((text (string-join (map strip-ansi (alt-screen-lines screen)) "\n")))
    (check-false (string-contains? text "Thinking blocks:"))
  ) ;let
) ;let*

(check-report)
