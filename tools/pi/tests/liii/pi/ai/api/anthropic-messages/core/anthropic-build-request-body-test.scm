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

(import (liii check)
  (liii pi ai api anthropic-messages core)
  (liii pi ai event)
) ;import

(check-set-mode! 'report-failed)

;; 完整请求体
(let* ((body (anthropic-build-request-body "claude-3-7-sonnet-20250219"
                                          #((("role" . "user") ("content" . "hi")))
                                          "You are a helpful assistant"
                                          '()
                                          4096)))
  (check (cdr (assoc "model" body)) => "claude-3-7-sonnet-20250219")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "system" body)) => "You are a helpful assistant")
  (check (vector-length (cdr (assoc "messages" body))) => 1)
  ;; 无 tools 时（空列表），请求体中不含 tools 字段
  (check (assoc "tools" body) => #f))

;; 空白系统提示词不写入 system 字段
(let ((body (anthropic-build-request-body "claude-test" #() "   " '() 1024)))
  (check (assoc "system" body) => #f)
  (check (cdr (assoc "max_tokens" body)) => 1024))

;; 缺省 max_tokens
(let ((body (anthropic-build-request-body "claude-test" #() #f '() #f)))
  (check (cdr (assoc "max_tokens" body)) => 8192))

;; 有工具声明时写入 tools 字段
(let ((body (anthropic-build-request-body "claude-test"
                     #()
                     #f
                     (list (list (cons "name" "bash")
                           (cons "description" "Run shell")
                           (cons "input_schema" '(("type" . "object")))))
                     1024)))
  (let ((tools (cdr (assoc "tools" body))))
    (check (vector-length tools) => 1)
    (check (cdr (assoc "name" (vector-ref tools 0))) => "bash")))

(check-report)
