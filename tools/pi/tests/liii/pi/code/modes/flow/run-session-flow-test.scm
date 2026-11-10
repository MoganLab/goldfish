;; run-session-flow
;; 单会话模式运行一轮对话：落盘消息、驱动 Agent 循环并回调结束处理。
;;
;; 语法
;; ----
;; (run-session-flow parsed cfg initial-msg emit on-finish session-file rest-msgs emit-initial?)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的命令行参数 alist。
;; cfg : alist
;; 运行配置 alist，含 tools、provider 与 config-dir。
;; initial-msg : string
;; 本轮用户消息文本。
;; emit : procedure | #f
;; 事件派发回调。
;; on-finish : procedure
;; 本轮结束回调，接收最终消息列表。
;; session-file : string | #f
;; 复用的会话文件路径；#f 表示新建会话文件。
;; rest-msgs : list
;; 本轮追加的用户消息文本列表。
;; emit-initial? : boolean
;; 是否派发初始消息的上屏事件。
;;
;; 返回值
;; ----
;; unspecified
;; 返回 on-finish 回调的结果。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii pi ai message)
  (liii pi code core compaction session)
  (liii pi code core session-manager)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define CONFIG-DIR (path-join (path-temp-dir) "pi-run-session-flow-test"))

(define SFILE
  (path->string (path-join (path-temp-dir) "pi-run-session-flow-test.jsonl"))
) ;define

(path-mkdir CONFIG-DIR :parents #t :exist-ok #t)

(when (file-exists? SFILE)
  (path-unlink (path SFILE))
) ;when

(save-session-header! SFILE "session-flow-test" "2026-01-01T00:00:00.000Z"
  "/tmp"
) ;save-session-header!

(define CFG
  (list (cons 'tools '())
    (cons 'provider "google")
    (cons 'model-id "gemini-3.8-flash")
    (cons 'config-dir CONFIG-DIR)
  ) ;list
) ;define

;; 1. 会话超时为负数：立即以超时错误结算，on-finish 收到用户消息与错误消息
(let* ((finished #f)
       (emitted '())
       (emit (lambda (ev-type . args) (set! emitted (cons ev-type emitted))))
       (on-finish (lambda (msgs) (set! finished msgs)))
       (parsed (list (cons 'session-timeout -1)))
       (res (run-session-flow parsed CFG "hello" emit on-finish SFILE))
      ) ;
  (check-true (pair? finished))
  (check (length finished) => 2)
  (check-true (pi-user-message? (car finished)))
  (check-true (pi-assistant-message? (cadr finished)))
  (check (pi-assistant-message-stop-reason (cadr finished)) => "error")
  ;; 事件派发：session 事件先行，随后为 Agent 事件
  (check-true (pair? (member 'session emitted)))
  (check-true (pair? (member 'agent_start emitted)))
) ;let*

;; 2. 用户消息与错误消息均已落盘
(let ((entries (pi-read-session-entries SFILE)))
  ;; 会话头 + 用户消息 + 错误消息
  (check (length entries) => 3)
  (check (cdr (assoc "type" (cadr entries))) => "message")
  (check
    (cdr
      (assoc "role" (cdr (assoc "message" (cadr entries))))
    ) ;cdr
    =>
    "user"
  ) ;check
  (check
    (cdr
      (assoc "role" (cdr (assoc "message" (caddr entries))))
    ) ;cdr
    =>
    "assistant"
  ) ;check
) ;let

(check-report)
