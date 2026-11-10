;; pi-app-poll-events!
;; 上下文占用显示：message_usage 事件驱动页脚左下角占比实时刷新，
;; compaction 事件后占比未知显示 "?"（对齐上游 pi 页脚）
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
;; ------
;; any
;; 执行事件消费与界面重绘的副作用。

(import (scheme base)
  (liii check)
  (liii string)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

;; 1. message_usage 事件携带上下文窗口：占比按单轮 usage 换算
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ch (pi-app-event-chan app))
       (usage '((input . 100000)
                (output . 5000)
                (cacheRead . 140000)
                (cacheWrite . 5000))
       ) ;usage
      ) ;
  (chan-send! ch (list 'message_usage usage 1000000))
  (pi-app-poll-events! app)
  (check (pi-footer-stats (%pi-app-footer app))
    =>
    "↑100k ↓5.0k R140k W5.0k CH57.1% 25.0%/1.0M (auto)"
  ) ;check
) ;let*

;; 2. 缺少 totalTokens 时按 input+output+cacheRead+cacheWrite 求和
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ch (pi-app-event-chan app))
       (usage '((input . 500000) (output . 0) (cacheRead . 0) (cacheWrite . 0)))
      ) ;
  (chan-send! ch (list 'message_usage usage 1000000))
  (pi-app-poll-events! app)
  (check-true (string-contains (pi-footer-stats (%pi-app-footer app)) "50.0%/1.0M (auto)")
  ) ;check-true
) ;let*

;; 3. compaction 事件后占比未知，显示 "?"
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ch (pi-app-event-chan app))
       (usage '((input . 250000) (output . 0) (cacheRead . 0) (cacheWrite . 0)))
      ) ;
  (chan-send! ch (list 'message_usage usage 1000000))
  (pi-app-poll-events! app)
  (check-true (string-contains (pi-footer-stats (%pi-app-footer app)) "25.0%/1.0M (auto)")
  ) ;check-true
  (chan-send! ch (list 'compaction))
  (pi-app-poll-events! app)
  (check-true (string-contains (pi-footer-stats (%pi-app-footer app)) "?/1.0M (auto)")
  ) ;check-true
) ;let*

(check-report)
