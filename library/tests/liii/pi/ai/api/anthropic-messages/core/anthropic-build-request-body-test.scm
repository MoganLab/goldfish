;; anthropic-build-request-body
;; 组装发送给 Anthropic API 的 JSON 请求体。
;;
;; 语法
;; ----
;; (anthropic-build-request-body model-id messages system-prompt tool-decls max-tokens)
;;
;; 参数
;; ----
;; model-id : string
;; 模型 id，写入请求体的 model 字段。
;; messages : vector
;; 已转换的 Anthropic API 消息向量。
;; system-prompt : string | #f
;; 系统提示词；为 #f 或空白字符串时省略 system 字段。
;; tool-decls : list
;; 工具声明列表；为空时省略 tools 字段。
;; max-tokens : integer | #f
;; 最大输出 token 数；为 #f 时使用 8192。
;;
;; 返回值
;; ----
;; alist
;; Anthropic API 请求体 alist，包含 model、max_tokens 与 messages 字段。
;;
;; 说明
;; ----
;; 为提升前缀缓存（prompt caching）命中率，请求体在三个位置挂
;; cache_control: {"type": "ephemeral"} 断点（对齐上游 pi 的实现）：
;; 1. system 提示词块（system 字段为 content block 数组而非纯字符串）
;; 2. 最后一个工具声明
;; 3. 最后一条 user 消息的最后一个 content 块
;; 断点之前的内容（tools + system + 历史 messages）跨轮字节级一致时，
;; 供应商可直接复用缓存前缀。

(import (liii check)
  (liii json)
  (liii string)
  (liii pi ai api anthropic-messages core)
  (liii pi ai event)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 辅助：取消息向量中最后一条消息最后一个 content 块
(define (%last-block messages)
  (let* ((last-msg (vector-ref messages (- (vector-length messages) 1)))
         (content (cdr (assoc "content" last-msg))))
    (vector-ref content (- (vector-length content) 1))))

;; 辅助：块上是否挂了 ephemeral cache_control 断点
(define (%has-cache-control? blk)
  (let ((cc (cdr (assoc "cache_control" blk))))
    (and (pair? cc) (equal? (cdr (assoc "type" cc)) "ephemeral"))))

;; 1. 完整请求体：system 块数组断点 + 最后一个 tool 断点 + 最后一条 user 消息断点
(let* ((ant-msgs (anthropic-build-messages
                   (list (pi-user-message "hello")
                     (pi-assistant-message (list (pi-text-block "hi")))
                     (pi-user-message "world"))))
       (body (anthropic-build-request-body "claude-3-7-sonnet-20250219"
                                           ant-msgs
                                           "You are a helpful assistant"
                                           (list (list (cons "name" "read")
                                                 (cons "description" "Read file")
                                                 (cons "input_schema" '(("type" . "object"))))
                                             (list (cons "name" "bash")
                                                 (cons "description" "Run shell")
                                                 (cons "input_schema" '(("type" . "object")))))
                                           4096)))
  (check (cdr (assoc "model" body)) => "claude-3-7-sonnet-20250219")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  ;; 1a. system 是 content block 数组，块上有 cache_control
  (let ((sys (cdr (assoc "system" body))))
    (check (vector? sys) => #t)
    (check (vector-length sys) => 1)
    (let ((blk (vector-ref sys 0)))
      (check (cdr (assoc "type" blk)) => "text")
      (check (cdr (assoc "text" blk)) => "You are a helpful assistant")
      (check-true (%has-cache-control? blk))))
  ;; 1b. tools 中仅最后一个声明挂 cache_control
  (let ((tools (cdr (assoc "tools" body))))
    (check (vector-length tools) => 2)
    (check (assoc "cache_control" (vector-ref tools 0)) => #f)
    (check-true (%has-cache-control? (vector-ref tools 1))))
  ;; 1c. messages 中仅最后一条消息的最后一个块挂 cache_control
  (check (vector-length (cdr (assoc "messages" body))) => 3)
  (check (assoc "cache_control"
            (vector-ref (cdr (assoc "content" (vector-ref (cdr (assoc "messages" body)) 0))) 0))
    =>
    #f
  ) ;check
  (check (assoc "cache_control"
            (vector-ref (cdr (assoc "content" (vector-ref (cdr (assoc "messages" body)) 1))) 0))
    =>
    #f
  ) ;check
  (check-true (%has-cache-control? (%last-block (cdr (assoc "messages" body)))))
) ;let*

;; 2. 最后一条消息以 tool_result 结尾（工具回合的常态）：断点落在 tool_result 块上
(let* ((ant-msgs (anthropic-build-messages
                   (list (pi-user-message "run ls")
                     (pi-assistant-message
                       (list (pi-tool-call-block "call_1" "bash" '(("command" . "ls")))))
                     (pi-tool-result-message "call_1" "bash" (list (pi-text-block "file.txt")) #f))))
       (body (anthropic-build-request-body "claude-test" ant-msgs "sys" '() 1024))
       (last-blk (%last-block (cdr (assoc "messages" body)))))
  (check (cdr (assoc "type" last-blk)) => "tool_result")
  (check-true (%has-cache-control? last-blk)))

;; 3. 空白系统提示词不写入 system 字段；消息断点不受影响
(let* ((body (anthropic-build-request-body "claude-test"
                     (anthropic-build-messages (list (pi-user-message "hi")))
                     "   "
                     '()
                     1024)))
  (check (assoc "system" body) => #f)
  (check-true (%has-cache-control? (%last-block (cdr (assoc "messages" body))))))

;; 4. 无工具声明时省略 tools 字段；消息断点不受影响
(let* ((body (anthropic-build-request-body "claude-test"
                     (anthropic-build-messages (list (pi-user-message "hi")))
                     #f
                     '()
                     1024)))
  (check (assoc "tools" body) => #f)
  (check-true (%has-cache-control? (%last-block (cdr (assoc "messages" body))))))

;; 5. 缺省 max_tokens
(let ((body (anthropic-build-request-body "claude-test" #() #f '() #f)))
  (check (cdr (assoc "max_tokens" body)) => 8192))

;; 6. 空消息向量：不加消息断点（system/tools 断点仍可存在），不抛错
(let ((body (anthropic-build-request-body "claude-test" #() "sys" '() 1024)))
  (check (vector-length (cdr (assoc "messages" body))) => 0))

;; 7. 手写 Anthropic 形态消息（content 为纯字符串）：末条字符串 content
;;    转为带断点的 text 块数组
(let* ((msgs #((("role" . "user") ("content" . "hi"))))
       (body (anthropic-build-request-body "claude-test" msgs "sys" '() 1024))
       (last (vector-ref (cdr (assoc "messages" body)) 0))
       (content (cdr (assoc "content" last))))
  (check (vector? content) => #t)
  (check (cdr (assoc "type" (vector-ref content 0))) => "text")
  (check (cdr (assoc "text" (vector-ref content 0))) => "hi")
  (check-true (%has-cache-control? (vector-ref content 0))))

;; 8. 序列化防回归：cache_control 嵌套对象正确输出为 {"type":"ephemeral"}
(let* ((body (anthropic-build-request-body "claude-test"
                     (anthropic-build-messages (list (pi-user-message "hi")))
                     "sys"
                     '()
                     1024))
       (json-str (json->string body)))
  (check-true (string-contains? json-str "\"cache_control\":{\"type\":\"ephemeral\"}")))

(check-report)
