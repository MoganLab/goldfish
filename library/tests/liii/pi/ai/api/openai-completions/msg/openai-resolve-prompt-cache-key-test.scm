;; openai-resolve-prompt-cache-key
;; 依据服务商 base-url 与会话标识解析 prompt_cache_key。
;;
;; 仅当 base-url 指向 OpenAI 官方端点（含 "api.openai.com"）且提供了会话标识时，
;; 返回截断到 64 字符的缓存键，使同一会话的请求路由到同一缓存分片，提升
;; 前缀缓存命中率（对齐上游 pi 的注入条件）；其余情况返回 #f 不注入，
;; 因为第三方 OpenAI 兼容端点对未知字段可能报错。
;;
;; 语法
;; ----
;; (openai-resolve-prompt-cache-key base-url session-id)
;;
;; 参数
;; ----
;; base-url : (or string? #f)
;; 服务商 base-url。
;; session-id : (or string? #f)
;; 会话标识（通常是会话文件路径），#f 表示无。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 满足注入条件时返回截断后的缓存键，否则 #f。

(import (liii check) (liii string) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

;; 1. OpenAI 官方端点 + 有会话标识：返回缓存键
(check (openai-resolve-prompt-cache-key "https://api.openai.com/v1" "sess-abc")
  =>
  "sess-abc"
) ;check

;; 2. 第三方端点：不注入（返回 #f）
(check (openai-resolve-prompt-cache-key "https://api.deepseek.com/v1" "sess-abc") => #f)
(check (openai-resolve-prompt-cache-key "https://api.moonshot.cn/v1" "sess-abc") => #f)
(check (openai-resolve-prompt-cache-key "http://127.0.0.1:1" "sess-abc") => #f)

;; 3. OpenAI 官方端点但无会话标识：不注入
(check (openai-resolve-prompt-cache-key "https://api.openai.com/v1" #f) => #f)
(check (openai-resolve-prompt-cache-key "https://api.openai.com/v1" "") => #f)

;; 4. base-url 缺省（#f）：不注入
(check (openai-resolve-prompt-cache-key #f "sess-abc") => #f)

;; 5. 超长会话标识：截断到 64 字符
(let ((long-id (string-append "/home/user/.gf/agent/sessions/" (make-string 100 #\s))))
  (check (string-length (openai-resolve-prompt-cache-key "https://api.openai.com/v1" long-id))
    =>
    64
  ) ;check
) ;let

(check-report)
