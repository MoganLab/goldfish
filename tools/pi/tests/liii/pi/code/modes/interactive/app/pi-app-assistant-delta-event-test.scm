;; pi-app-assistant-delta-event
;; 助手正文增量事件（assistant_text_delta / assistant_text_end）渲染与卡片归集测试。
;;
;; 语法
;; ----
;; (pi-app-poll-events! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; unspecified

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

;; 1. 跳过纯空白前导增量，首个有效增量建卡并去掉前导空白；后续增量原地追加到同一卡片
(let ((app (make-pi-app
             :terminal (make-virtual-terminal :width 100 :height 40)
             :handler  (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (send-event! app '(assistant_text_delta "\n\n"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 0)
  (check (pi-app-assistant-card app) => #f)

  (send-event! app '(assistant_text_delta "\n你好，"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 1)
  (let ((card (car (pi-app-chat app))))
    (check (pi-app-assistant-card app) => card)
    (check (assistant-card-text card) => "你好，")

    (send-event! app '(assistant_text_delta "世界！"))
    (pi-app-poll-events! app)
    (check (length (pi-app-chat app)) => 1)
    (check (pi-app-assistant-card app) => card)
    (check (assistant-card-text card) => "你好，世界！")
    (check-true (string-contains? (chat-text app) "你好，世界！"))
  ) ;let

  ;; assistant_text_end 关闭当前卡片引用；下一轮增量建新卡片
  (send-event! app '(assistant_text_end))
  (pi-app-poll-events! app)
  (check (pi-app-assistant-card app) => #f)

  (send-event! app '(assistant_text_delta "第二段"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 2)
  (check (assistant-card-text (cadr (pi-app-chat app))) => "第二段")
) ;let

;; 2. tool_execution_start 与 agent_settled 均关闭当前 assistant 卡片引用
(let ((app (make-pi-app
             :terminal (make-virtual-terminal :width 100 :height 40)
             :handler  (lambda (a m) #t)
           ) ;make-pi-app
      ) ;app
     ) ;
  (send-event! app '(assistant_text_delta "先查一下"))
  (pi-app-poll-events! app)
  (check-true (assistant-card-component? (pi-app-assistant-card app)))

  (send-event! app '(tool_execution_start "t1" "read" ((path . "a.txt"))))
  (pi-app-poll-events! app)
  (check (pi-app-assistant-card app) => #f)
  (check (length (pi-app-chat app)) => 2)

  (send-event! app '(assistant_text_delta "查完了"))
  (pi-app-poll-events! app)
  (check (length (pi-app-chat app)) => 3)
  (check-true (assistant-card-component? (pi-app-assistant-card app)))

  (send-event! app '(agent_settled #f))
  (pi-app-poll-events! app)
  (check (pi-app-assistant-card app) => #f)
) ;let

(check-report)
