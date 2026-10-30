(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

(define (send-event! app event)
  (chan-send! (pi-app-event-chan app) event 1000)
) ;define

(define (chat-text app)
  (string-join
    (map strip-ansi
      (append-map (lambda (card) (component-render card 80)) (pi-app-chat app))
    ) ;map
    "\n"
  ) ;string-join
) ;define

;; 首行建立 thinking 卡片，后续行原地追加；正文仍使用独立卡片。
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 100 :height 40)
              :handler (lambda (a m) #t)
            ) ;make-pi-app
       ) ;app
       (sv (pi-app-scroll-view app))
      ) ;
  (send-event! app '(assistant_thinking_line "思考第一行"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 1)
  (let ((card (car (pi-app-chat app))))
    (check (pi-app-thinking-card app) => card)
    (check-true (string-contains? (chat-text app) " 思考第一行"))

    ;; 缓存建立后新增行，滚动行数必须立即反映新内容。
    (let ((before (scroll-view-total-lines sv)))
      (send-event! app '(assistant_thinking_line "思考第二行"))
      (pi-app-poll-events! app)
      (check (length (pi-app-chat app)) => 1)
      (check (car (pi-app-chat app)) => card)
      (check (pi-app-thinking-card app) => card)
      (check (scroll-view-total-lines sv) => (+ before 1))
      (check-true (string-contains? (chat-text app) " 思考第二行"))
    ) ;let
  ) ;let
  (send-event! app '(assistant_text "正式回答"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 2)
  (check-true (string-contains? (chat-text app) "正式回答"))
) ;let*

;; 正常结算关闭活跃引用但保留旧内容；下一条消息重新建立卡片。
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 100 :height 40)
             :handler (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (send-event! app '(assistant_thinking_line "第一轮思考"))
  (pi-app-poll-events! app)
  (let ((first-card (pi-app-thinking-card app)))
    (send-event! app '(agent_settled #f))
    (pi-app-poll-events! app)
    (check-false (pi-app-thinking-card app))
    (check (length (pi-app-chat app)) => 1)

    (send-event! app '(assistant_thinking_line "第二轮思考"))
    (pi-app-poll-events! app)
    (check (length (pi-app-chat app)) => 2)
    (check-false (eq? first-card (pi-app-thinking-card app)))
    (check-true (string-contains? (chat-text app) " 第一轮思考"))
    (check-true (string-contains? (chat-text app) " 第二轮思考"))
  ) ;let
) ;let

;; 错误结算同样关闭活跃卡片。
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 100 :height 40)
             :handler (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (send-event! app '(assistant_thinking_line "出错前的思考"))
  (pi-app-poll-events! app)
  (send-event! app '(error "stream failed"))
  (pi-app-poll-events! app)
  (check-false (pi-app-thinking-card app))
  (send-event! app '(assistant_thinking_line "错误后的思考"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 3)
) ;let

;; 自定义 handler 同步接续下一任务时，出队前关闭上一轮 thinking 卡片。
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 100 :height 40)
             :handler (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (pi-app-submit! app "第一轮")
  (send-event! app '(assistant_thinking_line "第一轮思考"))
  (pi-app-poll-events! app)
  (let ((first-card (pi-app-thinking-card app)))
    (pi-app-submit! app "第二轮")
    (check-false (pi-app-thinking-card app))
    (send-event! app '(assistant_thinking_line "第二轮思考"))
    (pi-app-poll-events! app)
    (check-false (eq? first-card (pi-app-thinking-card app)))
    (check (length (pi-app-chat app)) => 4)
  ) ;let
) ;let

(check-report)
