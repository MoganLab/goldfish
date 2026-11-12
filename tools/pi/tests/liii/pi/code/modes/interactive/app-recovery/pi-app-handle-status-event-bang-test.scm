;; pi-app-handle-status-event!
;; 把 all-in-one 等待与思考等级收敛事件翻译为后台状态卡片。
;;
;; 语法
;; ----
;; (pi-app-handle-status-event! app ev)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; ev : list
;; 事件列表：(recovery_wait chain-id remaining-ms) /
;; (recovery_tick chain-id remaining-ms) / (recovery_retry chain-id) /
;; (recovery_end chain-id reason) / (thinking_clamp payload)。
;;
;; 返回值
;; ----
;; unspecified
;; recovery 事件原地更新同一张等待卡片；thinking_clamp 追加一次性提示卡片；
;; 两者都不写入会话 JSONL。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii tui component)
  (liii tui text-width)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app-recovery)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

(define (chat-text app)
  (apply string-append
    (map
      (lambda (card)
        (apply string-append (map strip-ansi (component-render card 200)))
      ) ;lambda
      (pi-app-chat app)
    ) ;map
  ) ;apply
) ;define

;; 1. wait 后连续 tick 只维护一张卡片，文本更新为最新剩余秒数
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-handle-status-event! app '(recovery_wait "coding-fast" 5000))
  (check (length (pi-app-chat app)) => 1)
  (check-true (string-contains? (chat-text app) "5s"))
  (pi-app-handle-status-event! app '(recovery_tick "coding-fast" 3000))
  (check (length (pi-app-chat app)) => 1)
  (check-true (string-contains? (chat-text app) "3s"))
  (check-false (string-contains? (chat-text app) "5s"))
) ;let

;; 2. recovery_retry 改写同一张卡片
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-handle-status-event! app '(recovery_wait "coding-fast" 5000))
  (pi-app-handle-status-event! app '(recovery_retry "coding-fast"))
  (check (length (pi-app-chat app)) => 1)
  (check-true (string-contains? (chat-text app) "Retrying all-in-one chain"))
) ;let

;; 3. recovery_end 成功：卡片保留最终文案并释放引用
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-handle-status-event! app '(recovery_wait "coding-fast" 5000))
  (pi-app-handle-status-event! app '(recovery_end "coding-fast" success))
  (check (length (pi-app-chat app)) => 1)
  (check-true (string-contains? (chat-text app) "recovered"))
  (check-false (pi-app-recovery-card app))
) ;let

;; 4. recovery_end 超时：卡片给出结束原因
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-handle-status-event! app '(recovery_wait "coding-fast" 2000))
  (pi-app-handle-status-event! app '(recovery_end "coding-fast" timeout))
  (check-true (string-contains? (chat-text app) "timeout"))
) ;let

;; 5. thinking_clamp：一次性提示卡片说明请求值与实际生效值
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-handle-status-event! app
    '(thinking_clamp ("google" "gemini-3.8-flash" "max" "high"))
  ) ;pi-app-handle-status-event!
  (check (length (pi-app-chat app)) => 1)
  (let ((text (chat-text app)))
    (check-true (string-contains? text "max"))
    (check-true (string-contains? text "high"))
    (check-true (string-contains? text "google/gemini-3.8-flash"))
  ) ;let
  ;; 收敛提示不占用等待卡片槽位
  (check-false (pi-app-recovery-card app))
) ;let

;; 6. thinking_clamp 载荷不完整时不产生卡片
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-handle-status-event! app '(thinking_clamp ("google")))
  (check (length (pi-app-chat app)) => 0)
) ;let

(check-report)
