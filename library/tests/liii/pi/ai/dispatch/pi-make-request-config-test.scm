;; pi-make-request-config
;; 构造标准请求配置关联表，根据 provider、model-id 和 api-key 自动推导默认端点与协议。
;;
;; 语法
;; ----
;; (pi-make-request-config provider model-id api-key [base-url] [provider-api] [max-tokens] [tools] [proxy-url] [settings])
;;
;; 参数
;; ----
;; provider : (or string? symbol?)
;; 服务商标识（如 "deepseek"、"google" 等）。
;;
;; model-id : string?
;; 模型标识符（如 "deepseek-chat"）。
;;
;; api-key : (or string? #f)
;; API 访问密钥。
;;
;; 可选命名参数:
;; :base-url (or string? #f) - 自定义 API 基础 URL，默认从服务商注册表读取
;; :provider-api (or string? #f) - 自定义协议标识，默认从服务商注册表读取
;; :max-tokens (or number? #f) - 最大生成 token 数，默认 4096
;; :tools list? - 工具定义列表，默认 '()
;; :proxy-url (or string? #f) - 代理地址，默认 #f
;; :settings (or list? #f) - 运行时额外设置，默认 '()
;;
;; 返回值
;; ----
;; alist?
;; 符合下游协议流式请求规范的配置关联表。

(import (liii check) (liii pi ai dispatch) (liii pi ai config record))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-make-request-config))

;; 1. 已注册服务商的默认推导
(let* ((cfg (pi-make-request-config "deepseek" "deepseek-chat" "sk-test"))
       (meta (cdr (assoc 'model-meta cfg)))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'provider-api cfg)) => "anthropic-messages")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-chat")
  (check (cdr (assoc 'api-key cfg)) => "sk-test")
  (check (pi-model-metadata-base-url meta) => "https://api.deepseek.com/anthropic")
  (check (pi-model-metadata-max-tokens meta) => 4096)
  (check (cdr (assoc 'tools cfg)) => '())
  (check (cdr (assoc 'proxy-url cfg)) => #f)
  (check (cdr (assoc 'settings cfg)) => '())
) ;let*

;; 2. 关键字参数覆盖
(let* ((cfg (pi-make-request-config "deepseek"
              "deepseek-chat"
              "sk-custom"
              :base-url     "https://api.deepseek.com"
              :provider-api "openai-completions"
              :max-tokens   8192
              :proxy-url    "http://127.0.0.1:7890"
              :tools        '(my-tool)
            ) ;pi-make-request-config
       ) ;cfg
       (meta (cdr (assoc 'model-meta cfg)))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'provider-api cfg)) => "openai-completions")
  (check (cdr (assoc 'api-key cfg)) => "sk-custom")
  (check (pi-model-metadata-base-url meta) => "https://api.deepseek.com")
  (check (pi-model-metadata-max-tokens meta) => 8192)
  (check (cdr (assoc 'proxy-url cfg)) => "http://127.0.0.1:7890")
  (check (cdr (assoc 'tools cfg)) => '(my-tool))
) ;let*

;; 3. 传入 symbol 作为 provider
(let ((cfg (pi-make-request-config 'google "gemini-2.5-flash" "key-123")))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'provider-api cfg)) => "google-generative-ai")
) ;let

;; 4. 未注册未知服务商回退默认
(let* ((cfg (pi-make-request-config "unknown-prov" "custom-m" "k"))
       (meta (cdr (assoc 'model-meta cfg)))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "unknown-prov")
  (check (cdr (assoc 'provider-api cfg)) => "google-generative-ai")
  (check (pi-model-metadata-base-url meta) => "")
) ;let*

(check-report)
