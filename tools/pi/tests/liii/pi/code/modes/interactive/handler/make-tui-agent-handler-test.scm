;; make-tui-agent-handler
;; 构造交互模式 Agent 消息处理函数。
;;
;; 语法
;; ----
;; (make-tui-agent-handler parsed cfg)
;; (make-tui-agent-handler parsed cfg runner)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析结果。
;; cfg : list?
;; 当前配置列表。
;; runner : procedure?
;; 可选的会话执行流函数，默认使用 run-session-flow。
;;
;; 返回值
;; ----
;; procedure?
;; 接收 (app user-text) 的消息处理函数。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive handler)
) ;import

(check-set-mode! 'report-failed)

;; 1. 当缺失认证信息时，向 app 注入 auth error 提示
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       ;; 传入空配置模拟缺少凭证
       (cfg '())
       (handler (make-tui-agent-handler '() cfg))
      ) ;
  (handler app "hello")
  (check-true (procedure? handler))
) ;let*

;; 2. 正常场景下调用 mock runner
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (called #f)
       (mock-runner (lambda (parsed cfg user-text emit on-done) (set! called #t)))
       (cfg '((api-key . "mock-key") (provider . "mock") (model-id . "mock")))
       (handler (make-tui-agent-handler '() cfg mock-runner))
      ) ;
  (handler app "test prompt")
  (check-true called)
) ;let*

(check-report)
