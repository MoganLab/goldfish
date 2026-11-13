;; pi-app-tool-timer-tick!
;; 运行中 bash 工具卡片 Elapsed 计时器节拍推进与自动重绘触发。
;;
;; 语法
;; ----
;; (pi-app-tool-timer-tick! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean?
;; 当且仅当有运行中的 bash 工具且自上次刷新满 1000ms 触发了界面重绘时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii string)
  (liii date-time current-time)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive views)
  (liii pi code modes interactive views tool-card)
  (liii pi code modes interactive app-tool-timer)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无运行中 bash 工具时：tick 返回 #f，无重绘副作用，last-tick 保持为 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 100 :height 30))))
  (check-false (pi-app-tool-timer-tick! app))
  (check-false (pi-app-tool-timer-last-tick app))
) ;let

;; 2. bash 工具运行期间：自动推进 Elapsed 计时
(let* ((vt (make-virtual-terminal :width 100 :height 30))
       (app (make-pi-app :terminal vt))
       (emit (make-tui-emit (pi-app-event-chan app)))
      ) ;
  ;; 启动 bash 工具调用
  (emit 'tool_execution_start "t-1" "bash" '(("command" . "make test")))
  (pi-app-poll-events! app)

  ;; 屏幕上初始显示 Elapsed 0.0s
  (let ((screen-text (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? screen-text "Elapsed 0.0s"))
  ) ;let

  ;; 首次 tick：记录当前时间戳，返回 #f（避免刚 start 又立刻重复渲染）
  (check-false (pi-app-tool-timer-tick! app))
  (check-true (number? (pi-app-tool-timer-last-tick app)))

  ;; 节拍未满 1000ms：不重绘，返回 #f
  (check-false (pi-app-tool-timer-tick! app))

  ;; 模拟经过 1000ms：tick 返回 #t 并触发重绘，卡片显示 Elapsed 1.0s
  (let* ((card (cdar (%pi-app-tool-cards app)))
         (now (current-monotonic-millis))
        ) ;
    (tool-card-set-started-at! card (- (%tool-now-ms) 1050))
    (pi-app-tool-timer-last-tick-set! app (- now 1000))
    (check-true (pi-app-tool-timer-tick! app))
    (let ((screen-text (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")))
      (check-true (string-contains? screen-text "Elapsed 1.0s"))
      (check-false (string-contains? screen-text "Elapsed 0.0s"))
    ) ;let

    ;; 再次模拟经过 2000ms（总计 3s）：卡片显示 Elapsed 3.0s
    (tool-card-set-started-at! card (- (%tool-now-ms) 3050))
    (pi-app-tool-timer-last-tick-set! app (- (current-monotonic-millis) 1000))
    (check-true (pi-app-tool-timer-tick! app))
    (let ((screen-text (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")))
      (check-true (string-contains? screen-text "Elapsed 3.0s"))
    ) ;let
  ) ;let*

  ;; 工具执行结束：结算为 Took，且后续 tick 复位为 #f 不再重绘
  (emit 'tool_execution_end "t-1" "bash" "Tests passed" #f 3200)
  (pi-app-poll-events! app)
  (let ((screen-text (string-join (map strip-ansi (alt-screen-lines (pi-app-screen app))) "\n")))
    (check-true (string-contains? screen-text "Took 3.2s"))
    (check-false (string-contains? screen-text "Elapsed"))
  ) ;let
  (check-false (pi-app-tool-timer-tick! app))
  (check-false (pi-app-tool-timer-last-tick app))
) ;let*

(check-report)
