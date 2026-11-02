;; pi-cfg-provider-proxy
;; 按 provider 重算生效代理：命令行 --proxy 优先于 provider 文件声明的 httpProxy。
;;
;; 语法
;; ----
;; (pi-cfg-provider-proxy cfg provider) -> string | #f
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
;; string | #f
;; 生效的代理 URL；未配置时返回 #f。

(import (liii check)
  (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

(define MODELS
  (list (cons "google" (list (cons "httpProxy" "http://provider:7890"))))
) ;define

(check (pi-cfg-provider-proxy (list (cons 'models-store MODELS)) "google")
  =>
  "http://provider:7890"
) ;check
(check (pi-cfg-provider-proxy
         (list (cons 'models-store MODELS) (cons 'cli-proxy "socks5h://cli:1080"))
         "google"
       ) ;pi-cfg-provider-proxy
  =>
  "socks5h://cli:1080"
) ;check

(check-report)
