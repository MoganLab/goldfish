;; pi-missing-credential-msg
;; 凭证缺失时生成用户可读提示消息，有凭证时返回 #f。
;;
;; 语法
;; ----
;; (pi-missing-credential-msg cfg)
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 生成的运行配置，读取 provider 与 api-key。
;;
;; 返回值
;; ----
;; string | #f
;; 缺少凭证时返回提示消息，否则返回 #f。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; 有非空 api-key 时不报错
(check (pi-missing-credential-msg '((provider . "google")
                                    (api-key . "secret-key")))
  =>
  #f
) ;check

;; api-key 为 #f 时返回带 provider 名的提示
(check (pi-missing-credential-msg '((provider . "kimi-coding") (api-key . #f)))
  =>
  "No credentials found for provider \"kimi-coding\""
) ;check

;; 空字符串视为缺少凭证
(check (pi-missing-credential-msg '((provider . "deepseek") (api-key . "")))
  =>
  "No credentials found for provider \"deepseek\""
) ;check

;; 缺少 provider 和 api-key 时回退默认 provider google
(check (pi-missing-credential-msg '())
  =>
  "No credentials found for provider \"google\""
) ;check

(check-report)
