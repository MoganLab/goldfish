;; pi-request-provider
;; 按供应商协议执行单次非流式请求，供进程内轻量探活等场景使用。
;;
;; 语法
;; ----
;; (pi-request-provider provider config msgs sys-prompt tools timeout [opt-retry])
;;
;; 参数
;; ----
;; provider : string
;; 供应商名称。
;; config : alist
;; 含 provider、provider-api、model-id、model-meta、api-key 等字段。
;; msgs : list
;; 待发送消息。
;; sys-prompt : string or #f
;; 系统提示词。
;; tools : list
;; 工具列表。
;; timeout : number
;; 超时秒数。
;; opt-retry : alist or #f (可选)
;; 重试覆盖配置；探活传 ((enabled . #f)) 可禁用自动重试。
;;
;; 返回值
;; ----
;; <pi-assistant-message>
;; 协议层返回的完整响应消息；失败时 stop-reason 为 error。

(import (scheme base) (liii check) (liii pi ai message) (liii pi ai dispatch))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-request-provider))

;; 明确指定未知测试供应商协议会走默认 Gemini 分支，失败也返回结构化助手消息。
(let* ((config (list (cons 'provider "nonexistent-probe-provider")
                 (cons 'provider-api "unknown-api")
                 (cons 'model-id "probe-model")
                 (cons 'model-meta (list (cons 'base-url "http://127.0.0.1:1")
                                     (cons 'max-tokens 16)
                                   ) ;list
                 ) ;cons
                 (cons 'api-key "test-key")
                 (cons 'proxy-url #f)
                 (cons 'settings '())
               ) ;list
       ) ;config
       (response
         (pi-request-provider "nonexistent-probe-provider"
           config
           (list (pi-user-message "ping"))
           #f
           '()
           1
           '((enabled . #f))
         ) ;pi-request-provider
       ) ;response
      ) ;
  (check-true (pi-assistant-message? response))
  (check (pi-assistant-message-stop-reason response) => "error")
  (check-true (string? (pi-assistant-message-error-message response)))
) ;let*

(check-report)
