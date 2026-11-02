;; pi-user-normalize-api
;; 把 provider 文件中的 api 简写归一化为完整协议标识。
;;
;; 语法
;; ----
;; (pi-user-normalize-api api-str)
;;
;; 参数
;; ----
;; api-str : string
;; provider 文件中声明的 api。
;;
;; 返回值
;; ----
;; string
;; 归一化后的协议标识；未知值原样返回。

(import (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(check (pi-user-normalize-api "anthropic") => "anthropic-messages")
(check (pi-user-normalize-api "openai") => "openai-completions")
(check (pi-user-normalize-api "google") => "google-generative-ai")
(check (pi-user-normalize-api "openai-completions") => "openai-completions")
(check (pi-user-normalize-api "custom-protocol") => "custom-protocol")

(check-report)
