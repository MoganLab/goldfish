;; pi-cfg-provider-api
;; 按运行配置中的 provider 声明取协议标识，缺失时回退注册表内置值。
;;
;; 语法
;; ----
;; (pi-cfg-provider-api cfg provider)
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 生成的运行配置，读取其中的 models-store。
;; provider : string
;; 供应商名称。
;;
;; 返回值
;; ----
;; string
;; provider 声明的协议标识，如 openai-completions 或 anthropic-messages。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; 自定义 provider 文件声明的 api 优先

(define CUSTOM-CFG
  '((models-store ("custom-kimi" ("api" . "openai-completions") ("models"))))
) ;define

(check (pi-cfg-provider-api CUSTOM-CFG "custom-kimi") => "openai-completions")

;; 未声明的 provider 回退注册表内置值
(check (pi-cfg-provider-api '() "google") => "google-generative-ai")
(check (pi-cfg-provider-api '() "deepseek") => "anthropic-messages")
(check (pi-cfg-provider-api '() "kimi-coding") => "openai-completions")

;; 未注册的 provider 默认走 google-generative-ai
(check (pi-cfg-provider-api '() "unknown-provider") => "google-generative-ai")

(check-report)
