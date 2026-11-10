;; pi-cfg-provider-api
;; 按 provider 取协议标识：优先使用目录声明的 api，缺失时回退注册表内置值。
;;
;; 语法
;; ----
;; (pi-cfg-provider-api cfg provider) -> string
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 返回的配置。
;; provider : string
;; provider id。
;;
;; 返回值
;; ----
;; string
;; 协议实现标识（如 google-generative-ai / openai-completions）。

(import (liii check) (liii pi ai providers registry) (liii pi code config))

(check-set-mode! 'report-failed)

(define CFG
  (list
    (cons 'models-store
      (list (cons "google" (list (cons "api" "openai-completions"))))
    ) ;cons
  ) ;list
) ;define

(check (pi-cfg-provider-api CFG "google") => "openai-completions")
(check (pi-cfg-provider-api '() "google") => (pi-provider-api "google"))
(check (pi-cfg-provider-api '() "google") => "google-generative-ai")

(check-report)
