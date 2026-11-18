;; pi-all-in-one-request-stream
;; 把元供应商请求经链式调度转成具体节点的真实协议请求。
;;
;; 语法
;; ----
;; (pi-all-in-one-request-stream provider config messages system-prompt
;;   tools on-event timeout [probe-fn request-fn])
;;
;; 参数
;; ----
;; provider : string
;; 元供应商标识 all-in-one。
;; config : <pi-config>
;; 主会话配置（本测试用 alist 形态的隔离夹具构造，含 config-dir、model-id 与 tools）。
;; messages : list
;; 主会话消息。
;; system-prompt : string
;; 主会话系统提示词。
;; tools、on-event、timeout : any
;; 原样传给真实请求函数。
;; probe-fn、request-fn : procedure (可选)
;; 测试用预检与真实请求函数。
;;
;; 返回值
;; ----
;; <pi-assistant-message>
;; 健康节点的真实响应；无可用链时返回错误消息。
;;
;; 说明
;; ----
;; 本文件同时覆盖全链枯竭的错误响应文案（stopReason=error，
;; 错误说明为 "All nodes in the all-in-one chain failed probe"），
;; 该文案构造器是 request 的内部实现，不单独公开导出。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi ai message)
  (liii pi code all-in-one request)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-request-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path (path-join TMP-DIR "models" "stub")) :parents #t :exist-ok #t)
(path-write-text (path->string (path-join TMP-DIR "all-in-one.json"))
  "{\"provider\":\"all-in-one\",\"probeTimeoutMs\":1234,\"continuous\":false,\"chains\":[{\"id\":\"coding-deep\",\"nodes\":[{\"provider\":\"stub\",\"model\":\"one\"},{\"provider\":\"stub\",\"model\":\"two\",\"thinkingLevel\":\"high\"}]}]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "providers.json"))
  "[\"stub\"]"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "auth.json"))
  "{\"stub\":{\"type\":\"api_key\",\"key\":\"fixture-key\"}}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub.json"))
  "{\"api\":\"openai-completions\",\"baseUrl\":\"http://127.0.0.1:1\",\"models\":[\"one\",\"two\"]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "one.json"))
  "{\"id\":\"one\",\"name\":\"one\"}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "two.json"))
  "{\"id\":\"two\",\"name\":\"two\",\"reasoning\":true,\"contextWindow\":234567}"
) ;path-write-text

(define CONFIG
  (list (cons 'provider "all-in-one")
    (cons 'provider-api "all-in-one")
    (cons 'model-id "coding-deep")
    (cons 'api-key #f)
    (cons 'config-dir TMP-DIR)
    (cons 'tools '())
  ) ;list
) ;define

(pi-chain-reset-state!)

;; 首节点真实请求失败、切换到第二节点（触发探活）；真实请求收到实际节点凭据与模型元数据，
;; 并在交接前用 active_node 事件告知前台当前节点
(let ((probed '()) (captured #f) (node-events '()))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            (cons
              (cons 'emit
                (lambda (type payload)
                  (set! node-events (cons (cons type payload) node-events))
                ) ;lambda
              ) ;cons
              CONFIG
            ) ;cons
            (list (pi-user-message "work"))
            "system"
            '(read)
            #f
            60
            (lambda (node timeout-ms config-dir run-fn)
              (set! probed (cons (chain-node-model node) probed))
              (check timeout-ms => 1234)
              (equal? (chain-node-model node) "two")
            ) ;lambda
            (lambda (provider config messages system-prompt tools on-event timeout)
              (if (equal? (cdr (assoc 'model-id config)) "one")
                (pi-assistant-message '() "error" "HTTP 503: unavailable")
                (begin
                  (set! captured (list provider config messages system-prompt tools timeout))
                  (pi-assistant-message (list (pi-text-block "ok")) "stop")
                ) ;begin
              ) ;if
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    ;; 首次请求不探活，仅在切换到 two 时探活
    (check (reverse probed) => '("two"))
    (check (pi-assistant-message-stop-reason response) => "stop")
    (check (car captured) => "stub")
    (let ((config (cadr captured)))
      (check (cdr (assoc 'provider config)) => "stub")
      (check (cdr (assoc 'provider-api config)) => "openai-completions")
      (check (cdr (assoc 'model-id config)) => "two")
      (check (cdr (assoc 'api-key config)) => "fixture-key")
      (check (cdr (assoc 'thinking-level config)) => "high")
    ) ;let
    (check (list-ref captured 3) => "system")
    (check (list-ref captured 4) => '(read))
    (check (list-ref captured 5) => 60)
    ;; 交接前已把实际节点回传给前台（含 supports-thinking?）
    (check
      (filter (lambda (p) (eq? (car p) 'active_node)) node-events)
      =>
      (list (cons 'active_node '("stub" "two" "high" 234567 #t))
        (cons 'active_node '("stub" "one" "off" 128000 #f))
      ) ;list
    ) ;check
  ) ;let
) ;let

;; 探活通过但真实请求失败时，清除当前粘性并顺延到下一健康节点
(pi-chain-reset-state!)
(let ((probed '()) (requested '()))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            CONFIG
            (list (pi-user-message "work"))
            "system"
            '()
            #f
            60
            (lambda (node timeout-ms config-dir request-fn)
              (set! probed (cons (chain-node-model node) probed))
              #t
            ) ;lambda
            (lambda (provider config messages system-prompt tools on-event timeout)
              (let ((model-id (cdr (assoc 'model-id config))))
                (set! requested (cons model-id requested))
                (if (equal? model-id "one")
                  (pi-assistant-message '() "error" "HTTP 503: unavailable")
                  (pi-assistant-message (list (pi-text-block "fallback ok")) "stop")
                ) ;if
              ) ;let
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "stop")
    (check (reverse probed) => '("two"))
    (check (reverse requested) => '("one" "two"))
  ) ;let
) ;let

;; 全链失败时返回错误响应（首节点真实请求失败、备选节点探活失败）
(pi-chain-reset-state!)
(let ((requested '()) (probed '()))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            CONFIG
            '()
            ""
            '()
            #f
            60
            (lambda (node . args) (set! probed (cons (chain-node-model node) probed)) #f)
            (lambda (provider config . args)
              (set! requested (cons (cdr (assoc 'model-id config)) requested))
              (pi-assistant-message '() "error" "HTTP 503")
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "error")
    (check (reverse requested) => '("one"))
    (check (reverse probed) => '("two"))
  ) ;let
) ;let

;; 主 cfg 中伪模型的 supports-thinking 不得覆盖节点真实能力：
;; all-in-one 元供应商自身通常为 #f，但节点 stub/two 声明 reasoning: true，
;; active_node 的第五项必须是节点的真实能力，否则页脚会按伪模型隐藏等级。
(pi-chain-reset-state!)
(let ((node-events '()))
  (pi-all-in-one-request-stream "all-in-one"
    (cons
      (cons 'emit
        (lambda (type . payload)
          (when (eq? type 'active_node)
            ;; emit 以 (type . args) 形式调用，payload 首项才是节点元组
            (set! node-events (cons (car payload) node-events))
          ) ;when
        ) ;lambda
      ) ;cons
      (cons (cons 'supports-thinking #f) CONFIG)
    ) ;cons
    (list (pi-user-message "work"))
    "system"
    '()
    #f
    60
    (lambda (node timeout-ms config-dir request-fn)
      (equal? (chain-node-model node) "two")
    ) ;lambda
    (lambda (provider node-cfg messages system-prompt tools on-event timeout)
      (if (equal? (cdr (assoc 'model-id node-cfg)) "one")
        (pi-assistant-message '() "error" "HTTP 503")
        (pi-assistant-message (list (pi-text-block "ok")) "stop")
      ) ;if
    ) ;lambda
  ) ;pi-all-in-one-request-stream
  (check (length node-events) => 2)
  (check (length (car node-events)) => 5)
  (check (list-ref (car node-events) 4) => #t)
) ;let

;; 链式调度中真实请求 429 失败耗尽后正确记录故障并切换备用节点：
(pi-chain-reset-state!)
(let ((executed '()))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one"
            (list (cons 'provider "all-in-one")
              (cons 'model-id "coding-deep")
              (cons 'config-dir TMP-DIR)
            ) ;list
            '()
            ""
            '()
            #f
            60
            (lambda (node timeout-ms config-dir request-fn) #t)
            (lambda (provider node-cfg messages system-prompt tools on-event timeout)
              (let ((m (cdr (assoc 'model-id node-cfg))))
                (set! executed (cons m executed))
                (if (equal? m "one")
                  (pi-assistant-message '()
                    :stop-reason   "error"
                    :error-message "HTTP 429 after 2 attempts"
                  ) ;pi-assistant-message
                  (pi-assistant-message (list (pi-text-block "switched-ok")) "stop")
                ) ;if
              ) ;let
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "stop")
    (check (reverse executed) => '("one" "two"))
  ) ;let
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
