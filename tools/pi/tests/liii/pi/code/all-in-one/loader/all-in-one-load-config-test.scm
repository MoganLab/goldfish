;; all-in-one-load-config
;; 加载并校验 all-in-one.json 配置文件。
;;
;; 语法
;; ----
;; (all-in-one-load-config . opt-config-dir)
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 可选配置目录，未提供或为 #f 时使用默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; (or <all-in-one-cfg> #f)
;; 成功时返回配置记录，文件不存在或校验失败时返回 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii shutil)
  (liii os)
  (liii pi code all-in-one types)
  (liii pi code all-in-one loader)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-loader-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (%write-cfg content)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
  (path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
  (let ((cfg-file (path-join TMP-DIR "all-in-one.json")))
    (call-with-output-file (path->string cfg-file) (lambda (p) (display content p)))
  ) ;let
) ;define

;; 1. 文件不存在返回 #f
(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(check (all-in-one-load-config TMP-DIR) => #f)

;; 2. JSON 语法错误返回 #f
(%write-cfg "{ broken json ...")
(check (all-in-one-load-config TMP-DIR) => #f)

;; 3. 根节点非对象返回 #f
(%write-cfg "[\"not\", \"object\"]")
(check (all-in-one-load-config TMP-DIR) => #f)

;; 4. 根节点 provider 不是 all-in-one 返回 #f
(%write-cfg "{\"provider\": \"openai\"}")
(check (all-in-one-load-config TMP-DIR) => #f)

;; 5. chains 非数组返回 #f
(%write-cfg "{\"provider\": \"all-in-one\", \"chains\": \"invalid\"}")
(check (all-in-one-load-config TMP-DIR) => #f)

;; 6. 完整配置加载与默认值回退
(%write-cfg "\n{\n  \"provider\": \"all-in-one\",\n  \"name\": \"测试高可用总线\",\n  \"continuous\": false,\n  \"maxWaitMs\": 120000,\n  \"retryIntervalMs\": 5000,\n  \"probeTimeoutMs\": 3000,\n  \"chains\": [\n    {\n      \"id\": \"coding-deep\",\n      \"name\": \"深度链\",\n      \"nodes\": [\n        {\"provider\": \"kimi1\", \"model\": \"kimi-k3\", \"thinkingLevel\": \"high\"},\n        {\"provider\": \"deepseek1\", \"model\": \"deepseek-chat\"}\n      ]\n    },\n    {\n      \"id\": \"coding-fast\",\n      \"nodes\": [\n        {\"provider\": \"minimax\", \"model\": \"MiniMax-M2\", \"thinkingLevel\": \"low\"}\n      ]\n    }\n  ]\n}\n"
) ;%write-cfg

(let ((cfg (all-in-one-load-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  (check (all-in-one-cfg-continuous? cfg) => #f)
  (check (all-in-one-cfg-max-wait-ms cfg) => 120000)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 5000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 3000)
  (let ((chains (all-in-one-cfg-chains cfg)))
    (check (length chains) => 2)
    ;; 链 1
    (let ((c1 (car chains)))
      (check (chain-def-id c1) => "coding-deep")
      (check (chain-def-name c1) => "深度链")
      (let ((nodes (chain-def-nodes c1)))
        (check (length nodes) => 2)
        (let ((n1 (car nodes)) (n2 (cadr nodes)))
          (check (chain-node-provider n1) => "kimi1")
          (check (chain-node-model n1) => "kimi-k3")
          (check (chain-node-thinking-level n1) => "high")
          ;; 缺省 thinkingLevel 回退到 "off"
          (check (chain-node-provider n2) => "deepseek1")
          (check (chain-node-model n2) => "deepseek-chat")
          (check (chain-node-thinking-level n2) => "off")
        ) ;let
      ) ;let
    ) ;let
    ;; 链 2 (缺省 name 使用 id)
    (let ((c2 (cadr chains)))
      (check (chain-def-id c2) => "coding-fast")
      (check (chain-def-name c2) => "coding-fast")
      (let ((nodes (chain-def-nodes c2)))
        (check (length nodes) => 1)
        (let ((n1 (car nodes)))
          (check (chain-node-provider n1) => "minimax")
          (check (chain-node-model n1) => "MiniMax-M2")
          (check (chain-node-thinking-level n1) => "low")
        ) ;let
      ) ;let
    ) ;let
  ) ;let
) ;let

;; 7. 坏节点跳过测试：节点缺 provider 或 model 时被跳过，合法节点仍保留
(%write-cfg "\n{\n  \"provider\": \"all-in-one\",\n  \"chains\": [\n    {\n      \"id\": \"mixed-chain\",\n      \"nodes\": [\n        {\"provider\": \"kimi1\"},\n        {\"model\": \"only-model\"},\n        \"not-an-object\",\n        {\"provider\": \"ds1\", \"model\": \"chat\"}\n      ]\n    }\n  ]\n}\n"
) ;%write-cfg

(let ((cfg (all-in-one-load-config TMP-DIR)))
  (check-true (all-in-one-cfg? cfg))
  ;; 验证全局参数缺省值
  (check (all-in-one-cfg-continuous? cfg) => #t)
  (check (all-in-one-cfg-max-wait-ms cfg) => 3600000)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 60000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 8000)
  (let ((chains (all-in-one-cfg-chains cfg)))
    (check (length chains) => 1)
    (let* ((c (car chains)) (nodes (chain-def-nodes c)))
      ;; 4 个节点中只有第 4 个合法
      (check (length nodes) => 1)
      (check (chain-node-provider (car nodes)) => "ds1")
      (check (chain-node-model (car nodes)) => "chat")
    ) ;let*
  ) ;let
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
