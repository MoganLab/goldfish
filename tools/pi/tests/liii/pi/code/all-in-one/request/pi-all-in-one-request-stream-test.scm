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
;; config : alist
;; 含 config-dir、model-id 和 tools 的主会话配置。
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
  "{\"provider\":\"all-in-one\",\"probeTimeoutMs\":1234,\"chains\":[{\"id\":\"coding-deep\",\"nodes\":[{\"provider\":\"stub\",\"model\":\"one\"},{\"provider\":\"stub\",\"model\":\"two\",\"thinkingLevel\":\"high\"}]}]}"
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

(pi-chain-reset-sticky-state!)

;; 首节点失败、第二节点成功；真实请求收到实际节点凭据与模型元数据，
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
              (set! captured (list provider config messages system-prompt tools timeout))
              (pi-assistant-message (list (pi-text-block "ok")) "stop")
            ) ;lambda
          ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (reverse probed) => '("one" "two"))
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
    ;; 交接前已把实际节点回传给前台
    (check
      (filter (lambda (p) (eq? (car p) 'active_node)) node-events)
      =>
      (list (cons 'active_node '("stub" "two" "high" 234567)))
    ) ;check
  ) ;let
) ;let

;; 探活通过但真实请求失败时，清除当前粘性并顺延到下一健康节点
(pi-chain-reset-sticky-state!)
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
    (check (reverse probed) => '("one" "two"))
    (check (reverse requested) => '("one" "two"))
  ) ;let
) ;let

;; 全链失败或链不存在时不调用真实请求函数
(pi-chain-reset-sticky-state!)
(let ((called? #f))
  (let ((response (pi-all-in-one-request-stream "all-in-one"
                    CONFIG
                    '()
                    ""
                    '()
                    #f
                    60
                    (lambda args #f)
                    (lambda args (set! called? #t))
                  ) ;pi-all-in-one-request-stream
        ) ;response
       ) ;
    (check (pi-assistant-message-stop-reason response) => "error")
    (check-false called?)
  ) ;let
) ;let

(rmtree TMP-DIR #t)

(check-report)
