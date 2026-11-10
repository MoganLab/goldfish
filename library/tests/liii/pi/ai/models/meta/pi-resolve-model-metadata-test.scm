;; pi-resolve-model-metadata
;; 解析模型的 base URL、上下文窗口和最大输出 token 等元数据。
;;
;; 语法
;; ----
;; (pi-resolve-model-metadata provider model-id models-store)
;; (pi-resolve-model-metadata provider model-id models-store opt-env)
;;
;; 参数
;; ----
;; provider : string? 或 symbol?
;; 模型供应商名称。
;;
;; model-id : string?
;; 模型标识。
;;
;; models-store : alist?
;; 模型元数据存储目录。
;;
;; opt-env : list?
;; 可选环境变量表。
;;
;; 返回值
;; ----
;; pi-model-metadata?
;; 包含模型名称、provider、base-url、context-window 和 max-tokens 的 record。

(import (liii check) (liii pi ai config record) (liii pi ai models meta))

(check-set-mode! 'report-failed)

(let ((m (pi-resolve-model-metadata "kimi-coding" "unlisted" '())))
  (check (pi-model-metadata? m) => #t)
  (check (pi-model-metadata-id m) => "unlisted")
  (check (pi-model-metadata-provider m) => "kimi-coding")
  (check (pi-model-metadata-base-url m) => "https://api.kimi.com/coding/v1")
  (check (pi-model-metadata-context-window m) => 262144)
  (check (pi-model-metadata-matched? m) => #f)
) ;let

(let ((m (pi-resolve-model-metadata "opencode" "unlisted" '())))
  (check (pi-model-metadata? m) => #t)
  (check (pi-model-metadata-id m) => "unlisted")
  (check (pi-model-metadata-provider m) => "opencode")
  (check (pi-model-metadata-base-url m) => "https://opencode.ai/zen/go/v1")
  (check (pi-model-metadata-context-window m) => 1000000)
  (check (pi-model-metadata-matched? m) => #f)
) ;let

(check-report)
