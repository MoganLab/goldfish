;; pi-resolve-provider-key
;; 解析指定服务商的 API 密钥（优先环境变量，其次 auth 配置，最后 fallback）。
;;
;; 语法
;; ----
;; (pi-resolve-provider-key provider auth-alist)
;; (pi-resolve-provider-key provider auth-alist opt-env)
;;
;; 参数
;; ----
;; provider : string?
;; 服务商名称（如 "deepseek"、"zai-coding-cn" 等）。
;;
;; auth-alist : list?
;; 已加载的凭据列表或 key-value 关联表。
;;
;; opt-env : list?
;; 可选，自定义环境变量列表。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 服务商对应的有效 API 密钥字符串；若未找到则返回 #f。

(import (liii check) (liii pi ai auth))

(check-set-mode! 'report-failed)

;; 1. 优先从环境变量读取
(let ((env '(("DEEPSEEK_API_KEY" . "env-secret-ds"))))
  (check (pi-resolve-provider-key "deepseek" '() env) => "env-secret-ds")
) ;let

;; 2. 环境变量未提供时从 auth-alist 读取
(let ((auth '(("deepseek" . "auth-file-key"))))
  (check (pi-resolve-provider-key "deepseek" auth '()) => "auth-file-key")
) ;let

;; 3. 服务商别名支持（zai-coding-cn ↔ zai）
(let ((auth '(("zai" . "zai-secret"))))
  (check (pi-resolve-provider-key "zai-coding-cn" auth '()) => "zai-secret")
) ;let

;; 4. 未找到时返回 #f
(check (pi-resolve-provider-key "unknown-provider" '() '()) => #f)

;; 5. ollama 免密：当无环境变量且无 auth 配置时，回退到默认虚拟密钥 "ollama"
(check (pi-resolve-provider-key "ollama" '() '()) => "ollama")
(let ((env '(("OLLAMA_API_KEY" . "custom-ollama-key"))))
  (check (pi-resolve-provider-key "ollama" '() env) => "custom-ollama-key")
) ;let
(let ((auth '(("ollama" . "auth-ollama-key"))))
  (check (pi-resolve-provider-key "ollama" auth '()) => "auth-ollama-key")
) ;let

(check-report)
