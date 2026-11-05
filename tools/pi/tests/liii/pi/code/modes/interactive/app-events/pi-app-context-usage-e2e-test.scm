;; 上下文占用的端到端链路：
;; agent loop 结算 → worker 事件桥 → 前台事件处理 → 页脚左下角占比
;;
;; 语法
;; ----
;; （本测试文件不导出函数，仅驱动完整链路并断言页脚显示）
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii string)
  (liii go)
  (liii tui)
  (liii pi ai message)
  (liii pi agent loop)
  (liii pi code modes interactive)
  (liii pi code modes interactive state)
  (liii pi code modes interactive footer)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 一轮 assistant 响应后，页脚显示真实上下文占比
(let* ((app (make-pi-app
              :terminal (make-virtual-terminal 100 40)
              :handler  (lambda (a m) #t)
              :context-window 1000000
            ) ;make-pi-app
           ) ;app
       (emit (make-tui-emit (pi-app-event-chan app) 1000000))
       (cfg '((provider . "test") (tools) (settings)))
       (usage '((input . 100000) (output . 20000) (cacheRead . 0) (cacheWrite . 0)
                 (totalTokens . 120000)))
       (msg (pi-assistant-message (list (pi-text-block "ok")) :usage usage))
       (request-fn (lambda (provider config msgs sys-prompt tools on-ev timeout) msg)
       ) ;request-fn
      ) ;
  (run-agent-loop cfg '() "sys" emit 10 5 600 #f request-fn #f)
  (pi-app-poll-events! app)
  (check (pi-footer-stats (%pi-app-footer app))
    => "↑100k ↓20k 12.0%/1.0M (auto)")
) ;let*

;; 2. 压缩事件后占比未知，显示 "?"
(let* ((app (make-pi-app
              :terminal (make-virtual-terminal 100 40)
              :handler  (lambda (a m) #t)
              :context-window 1000000
            ) ;make-pi-app
           ) ;app
       (emit (make-tui-emit (pi-app-event-chan app) 1000000))
       (cfg '((provider . "test") (tools) (settings)))
       (usage '((input . 100000) (output . 20000) (cacheRead . 0) (cacheWrite . 0)
                 (totalTokens . 120000)))
       (msg (pi-assistant-message (list (pi-text-block "ok")) :usage usage))
       (request-fn (lambda (provider config msgs sys-prompt tools on-ev timeout) msg)
       ) ;request-fn
      ) ;
  (run-agent-loop cfg '() "sys" emit 10 5 600 #f request-fn #f)
  (pi-app-poll-events! app)
  (check (pi-footer-stats (%pi-app-footer app))
    => "↑100k ↓20k 12.0%/1.0M (auto)")
  ;; 上下文重建：占比未知
  (chan-send! (pi-app-event-chan app) (list 'compaction))
  (pi-app-poll-events! app)
  (check (pi-footer-stats (%pi-app-footer app))
    => "↑100k ↓20k ?/1.0M (auto)")
) ;let*

(check-report)
