;; run-agent-loop-with-timeout
;; 以解析出的超时与轮数上限驱动 Agent 主循环。
;;
;; 语法
;; ----
;; (run-agent-loop-with-timeout cfg init-msgs system-prompt emit parsed
;;   compaction emit-initial? probe-fn request-fn)
;;
;; 参数
;; ----
;; cfg : alist
;; 运行配置 alist，含 tools、provider 与 model-id。
;; init-msgs : list
;; 初始消息列表。
;; system-prompt : string | procedure
;; 系统提示词文本或回调。
;; emit : procedure | #f
;; 事件派发回调。
;; parsed : alist
;; 解析后的命令行参数 alist；session-timeout 为负数时立即以超时错误结算。
;; compaction : list | #f
;; 上下文压缩配置。
;; emit-initial? : boolean
;; 是否派发初始消息的上屏事件。
;; probe-fn、request-fn : procedure | #f
;; all-in-one 隔离测试用预检与真实请求函数。
;;
;; 返回值
;; ----
;; list
;; 执行完成后的完整消息历史列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi ai message)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define CFG
  (list (cons 'tools '())
    (cons 'provider "google")
    (cons 'model-id "gemini-3.8-flash")
  ) ;list
) ;define

;; 统计指定类型事件的派发次数

(define (count-events evs type)
  (let loop
    ((rest evs) (n 0))
    (if (null? rest) n (loop (cdr rest) (if (eq? (car rest) type) (+ n 1) n)))
  ) ;let
) ;define

;; 1. 会话超时为负数：立即以超时错误结算，不发起模型请求
(let* ((parsed (list (cons 'session-timeout -1)))
       (res (run-agent-loop-with-timeout CFG (list (pi-user-message "hi")) "" #f parsed)
       ) ;res
      ) ;
  (check (length res) => 2)
  (check-true (pi-user-message? (car res)))
  (check-true (pi-assistant-message? (cadr res)))
  (check (pi-assistant-message-stop-reason (cadr res)) => "error")
  (check (pi-assistant-message-error-message (cadr res))
    =>
    "Session timeout exceeded: total execution deadline reached"
  ) ;check
) ;let*

;; 2. 事件派发：初始消息上屏与错误结算事件均已派发
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (parsed (list (cons 'session-timeout -1)))
       (res (run-agent-loop-with-timeout CFG (list (pi-user-message "hi")) "" emit parsed)
       ) ;res
      ) ;
  (check (length res) => 2)
  (check-true (pair? (member 'agent_start evs)))
  (check-true (pair? (member 'message_start evs)))
  (check-true (pair? (member 'message_end evs)))
  (check-true (pair? (member 'agent_end evs)))
) ;let*

;; 3. emit-initial? 为 #f 时不派发初始消息的上屏事件（仅剩错误消息一条）
(let* ((evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (parsed (list (cons 'session-timeout -1)))
       (res (run-agent-loop-with-timeout CFG
              (list (pi-user-message "hi"))
              ""
              emit
              parsed
              #f
              #f
            ) ;run-agent-loop-with-timeout
       ) ;res
       (msg-starts (count-events evs 'message_start))
      ) ;
  (check (length res) => 2)
  (check msg-starts => 1)
) ;let*

;; 4. all-in-one 必须进入链式调度，不再交给真实协议直接请求元供应商
(let* ((dir (path->string (path-join (path-temp-dir)
                          (string-append "aio-flow-test-" (number->string (getpid)))
                        ) ;path-join
            ) ;path->string
       ) ;dir
       (cfg (list (cons 'tools '())
              (cons 'provider "all-in-one")
              (cons 'model-id "coding-deep")
              (cons 'config-dir dir)
            ) ;list
       ) ;cfg
       (probed '())
       (requests '())
      ) ;
  (when (file-exists? dir)
    (rmtree dir #t)
  ) ;when
  (path-mkdir (path (path-join dir "models" "stub")) :parents #t :exist-ok #t)
  (path-write-text (path->string (path-join dir "models" "providers.json"))
    "[\"stub\"]"
  ) ;path-write-text
  (path-write-text (path->string (path-join dir "auth.json"))
    "{\"stub\":{\"type\":\"api_key\",\"key\":\"fixture-key\"}}"
  ) ;path-write-text
  (path-write-text (path->string (path-join dir "models" "stub.json"))
    "{\"api\":\"openai-completions\",\"baseUrl\":\"http://127.0.0.1:1\",\"models\":[\"m1\",\"m2\"]}"
  ) ;path-write-text
  (path-write-text (path->string (path-join dir "models" "stub" "m1.json"))
    "{\"name\":\"m1\"}"
  ) ;path-write-text
  (path-write-text (path->string (path-join dir "models" "stub" "m2.json"))
    "{\"name\":\"m2\",\"reasoning\":true}"
  ) ;path-write-text
  (path-write-text (path->string (path-join dir "all-in-one.json"))
    "{\"provider\":\"all-in-one\",\"probeTimeoutMs\":1000,\"chains\":[{\"id\":\"coding-deep\",\"nodes\":[{\"provider\":\"stub\",\"model\":\"m1\"},{\"provider\":\"stub\",\"model\":\"m2\",\"thinkingLevel\":\"high\"}]}]}"
  ) ;path-write-text
  (let ((res (run-agent-loop-with-timeout cfg (list (pi-user-message "hi")) "" #f '()
               #f #t
               (lambda (node timeout-ms config-dir run-fn)
                 (set! probed (cons (chain-node-provider node) probed))
                 (equal? (chain-node-model node) "m2")
               ) ;lambda
               (lambda (provider config messages system-prompt tools on-event timeout)
                 (set! requests (cons (list provider config messages) requests))
                 (pi-assistant-message (list (pi-text-block "pong")) "stop")
               ) ;lambda
             ) ;run-agent-loop-with-timeout
        ) ;res
       ) ;
    (check (reverse probed) => '("stub" "stub"))
    (check (length requests) => 1)
    (check (caar requests) => "stub")
    (check (cdr (assoc 'provider (cadar requests))) => "stub")
    (check (cdr (assoc 'model-id (cadar requests))) => "m2")
    (check (cdr (assoc 'thinking-level (cadar requests))) => "high")
    (check (pi-assistant-message-stop-reason (car (reverse res))) => "stop")
  ) ;let
  (rmtree dir #t)
) ;let*

(check-report)
