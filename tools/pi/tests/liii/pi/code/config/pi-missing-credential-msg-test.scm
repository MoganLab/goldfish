;; pi-missing-credential-msg
;; 凭证缺失时返回用户可读提示消息，否则返回 #f。
;;
;; 语法
;; ----
;; (pi-missing-credential-msg cfg) -> string | #f
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 返回的配置。
;;
;; 返回值
;; ----
;; string | #f
;; 缺少 api-key 时返回提示消息；凭证存在时返回 #f。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

(check (pi-missing-credential-msg '((provider . "google") (api-key . "secret")))
  =>
  #f
) ;check
(check (pi-missing-credential-msg '((provider . "google") (api-key . "")))
  =>
  "No credentials found for provider \"google\""
) ;check
(check (pi-missing-credential-msg '((provider . "kimi-coding") (api-key . #f)))
  =>
  "No credentials found for provider \"kimi-coding\""
) ;check
(check (pi-missing-credential-msg '())
  =>
  "No credentials found for provider \"google\""
) ;check
(check (pi-missing-credential-msg '((provider . "ollama") (api-key . #f)))
  =>
  #f
) ;check
(check (pi-missing-credential-msg '((provider . "ollama") (api-key . "")))
  =>
  #f
) ;check

(check-report)
