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
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi ai message)
  (liii pi code all-in-one request)
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
  "{\"id\":\"two\",\"name\":\"two\",\"reasoning\":true}"
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

;; 首节点失败、第二节点成功；真实请求收到实际节点凭据与模型元数据
(let ((probed '()) (captured #f))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one" CONFIG (list (pi-user-message "work"))
            "system" '(read) #f 60
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
  ) ;let
) ;let

;; 全链失败或链不存在时不调用真实请求函数
(let ((called? #f))
  (let ((response
          (pi-all-in-one-request-stream "all-in-one" CONFIG '() "" '() #f 60
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
