;; recovery 等待状态卡片的 TUI 端到端测试
;; 事件桥把 recovery 事件翻译成可原地更新的单张状态卡片，结算后不再追加。
;;
;; 语法
;; ----
;; (pi-build-json-event type args) / (make-tui-emit event-chan) /
;; (pi-app-poll-events! app) / (component-render comp width)
;;
;; 参数
;; ----
;; type : symbol，协议事件类型。
;; args : list，事件参数。
;; event-chan : chan，TUI 事件通道。
;; app : pi-app?，交互式应用实例。
;; comp : component?，待渲染组件。
;; width : integer，渲染宽度。
;;
;; 返回值
;; ----
;; #f 或事件/渲染结果
;; recovery 事件不属于协议事件（pi-build-json-event 返回 #f），
;; 因此不会写入 JSON 事件流；TUI 侧连续 tick 只更新同一张卡片。

(import (scheme base)
  (scheme write)
  (liii check)
  (liii go)
  (liii string)
  (liii tui)
  (liii pi ai event)
  (liii tui component)
  (liii pi code modes interactive)
  (liii pi code modes interactive worker-emit)
  (liii pi code modes interactive app-events)
  (liii pi code modes interactive state)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 1. recovery 事件不是协议事件：pi-build-json-event 对其返回 #f，
;;    因此 JSON/print 模式不会把倒计时混入 stdout
(check-false (pi-build-json-event 'recovery_wait '("coding-fast" 500 1000)))
(check-false (pi-build-json-event 'recovery_tick '("coding-fast" 500)))
(check-false (pi-build-json-event 'recovery_retry '("coding-fast")))
(check-false (pi-build-json-event 'recovery_end '("coding-fast" 'success)))

;; 2. 事件桥把 recovery 事件原样转发为 TUI 事件（单事件、带参数）
(let* ((ch (make-chan 16)) (emit (make-tui-emit ch)))
  (emit 'recovery_wait "coding-fast" 3000 3000)
  (emit 'recovery_tick "coding-fast" 2000)
  (emit 'recovery_retry "coding-fast")
  (emit 'recovery_end "coding-fast" 'success)
  (check (chan-try-recv! ch 'none) => '(recovery_wait "coding-fast" 3000 3000))
  (check (chan-try-recv! ch 'none) => '(recovery_tick "coding-fast" 2000))
  (check (chan-try-recv! ch 'none) => '(recovery_retry "coding-fast"))
  (check (chan-try-recv! ch 'none) => '(recovery_end "coding-fast" success))
  (check (chan-try-recv! ch 'none) => 'none)
) ;let*

;; 3. TUI 侧：反复 tick 只更新同一张状态卡片，不追加多张
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt))
      ) ;
  (pi-app-poll-events! app)
  (let ((ch (pi-app-event-chan app)))
    (chan-send! ch '(recovery_wait "coding-fast" 5000 5000) 1000)
    (chan-send! ch '(recovery_tick "coding-fast" 4000) 1000)
    (chan-send! ch '(recovery_tick "coding-fast" 3000) 1000)
    (pi-app-poll-events! app)
    ;; 只新增一张等待卡片
    (check (length (pi-app-chat app)) => 1)
    ;; 卡片文本随 tick 原地更新为最新剩余秒数
    (let* ((card (car (pi-app-chat app)))
           (lines (component-render card 80))
           (text (apply string-append (map strip-ansi lines)))
          ) ;
      (check-true (string-contains? text "3s"))
      (check-false (string-contains? text "5s"))
    ) ;let*
    ;; 恢复结束后卡片保留为最终状态，且不再追加新卡片
    (chan-send! ch '(recovery_end "coding-fast" success) 1000)
    (pi-app-poll-events! app)
    (check (length (pi-app-chat app)) => 1)
    (let* ((card (car (pi-app-chat app)))
           (text (apply string-append (map strip-ansi (component-render card 80))))
          ) ;
      (check-true (string-contains? text "recovered"))
    ) ;let*
  ) ;let
) ;let*

(check-report)
