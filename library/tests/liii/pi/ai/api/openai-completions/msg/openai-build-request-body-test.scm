;; openai-build-request-body
;; 组装 OpenAI Chat Completions 请求体 alist。
;;
;; 语法
;; ----
;; (openai-build-request-body model-id messages stream? tool-decls max-tokens
;;   (prompt-cache-key #f) (thinking-level #f) (model-entry #f))
;;
;; 参数
;; ----
;; model-id : string
;; 模型 id。
;; messages : vector
;; 已转换的 OpenAI messages 向量。
;; stream? : boolean
;; 是否开启流式传输。
;; tool-decls : list
;; 工具声明列表；为空时省略 tools 字段。
;; max-tokens : integer | #f
;; 最大输出 token 数；非正数或 #f 时省略。
;; prompt-cache-key : (or string? #f)
;; 可选，OpenAI 官方端点的 prompt_cache_key，同会话路由到同一缓存分片；
;; 非字符串时省略。
;; thinking-level : (or string? #f)
;; 可选，思考级别；配合 model-entry 生成 thinking / reasoning_effort 字段。
;; model-entry : (or alist? #f)
;; 可选，模型目录原始条目；提供 reasoning、thinkingLevelMap 与 compat 字段。
;;
;; 返回值
;; ----
;; alist
;; OpenAI API 请求体 alist。

(import (liii check) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (tools '((("type" . "function") ("function" . (("name" . "bash"))))))
       (body (openai-build-request-body "kimi-for-coding" msgs #t tools 4096)))
  (check (cdr (assoc "model" body)) => "kimi-for-coding")
  (check (cdr (assoc "messages" body)) => msgs)
  (check (cdr (assoc "stream" body)) => #t)
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "tools" body)) => (list->vector tools)))

;; 无 tools 和非流式
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "kimi-for-coding" msgs #f '() #f)))
  (check (cdr (assoc "model" body)) => "kimi-for-coding")
  (check (assoc "stream" body) => #f)
  (check (assoc "tools" body) => #f))

;; prompt-cache-key：传入非空字符串时写入 prompt_cache_key 字段
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "gpt-5.2" msgs #t '() 4096 "sess-abc")))
  (check (cdr (assoc "prompt_cache_key" body)) => "sess-abc"))

;; prompt-cache-key：缺省或 #f 时省略 prompt_cache_key 字段
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "gpt-5.2" msgs #t '() 4096)))
  (check (assoc "prompt_cache_key" body) => #f)
  (let ((body2 (openai-build-request-body "gpt-5.2" msgs #t '() 4096 #f)))
    (check (assoc "prompt_cache_key" body2) => #f)))

;; ---- thinking / reasoning 请求字段 ----

(define DEEPSEEK-MODEL
  '(("id" . "deepseek-ai/deepseek-v4.1-flash-1017-maas")
    ("reasoning" . true)
    ("thinkingLevelMap" ("low" . "low") ("medium" . null)
      ("high" . "high") ("max" . "max")
    ) ;thinkingLevelMap
    ("compat" ("thinkingFormat" . "deepseek") ("supportsReasoningEffort" . true))
   ) ;list
) ;define

(define OPENAI-MODEL
  '(("id" . "reasoning-model")
    ("reasoning" . true)
    ("compat" ("thinkingFormat" . "openai"))
   ) ;list
) ;define

(define NO-REASONING-MODEL
  '(("id" . "plain-model")
    ("reasoning" . false)
    ("compat" ("thinkingFormat" . "deepseek"))
   ) ;list
) ;define

;; deepseek 格式：非 off 级别同时写入 thinking.type 与 reasoning_effort
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "m" msgs #f '() #f #f "high" DEEPSEEK-MODEL)))
  (check (cdr (assoc "thinking" body)) => '(("type" . "enabled")))
  (check (cdr (assoc "reasoning_effort" body)) => "high"))

;; deepseek 格式：thinkingLevelMap 映射生效
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "m" msgs #f '() #f #f "low" DEEPSEEK-MODEL)))
  (check (cdr (assoc "reasoning_effort" body)) => "low"))

;; deepseek 格式：off 写入 thinking.type=disabled，且不写 reasoning_effort
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "m" msgs #f '() #f #f "off" DEEPSEEK-MODEL)))
  (check (cdr (assoc "thinking" body)) => '(("type" . "disabled")))
  (check (assoc "reasoning_effort" body) => #f))

;; openai 格式：只写 reasoning_effort，不写 thinking
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "m" msgs #f '() #f #f "high" OPENAI-MODEL)))
  (check (assoc "thinking" body) => #f)
  (check (cdr (assoc "reasoning_effort" body)) => "high"))

;; reasoning=false：不注入任何思考字段
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "m" msgs #f '() #f #f "high" NO-REASONING-MODEL)))
  (check (assoc "thinking" body) => #f)
  (check (assoc "reasoning_effort" body) => #f))

;; 未提供 model-entry：保持旧行为，不注入思考字段
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "m" msgs #f '() #f #f "high")))
  (check (assoc "thinking" body) => #f)
  (check (assoc "reasoning_effort" body) => #f))

(check-report)
