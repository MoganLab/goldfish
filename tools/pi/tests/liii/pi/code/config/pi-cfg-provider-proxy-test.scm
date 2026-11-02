;; pi-cfg-provider-proxy
;; 按运行配置重算指定 provider 的生效代理。
;;
;; 语法
;; ----
;; (pi-cfg-provider-proxy cfg provider)
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 生成的运行配置，含 models-store、settings 与 cli-proxy。
;; provider : string
;; 供应商名称。
;;
;; 返回值
;; ----
;; string | #f
;; 生效的代理 URL；没有任何代理时返回 #f。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

(define CFG
  '((models-store ("kimi-coding" ("httpProxy" . "http://provider-proxy:8080")))
    (settings (http-proxy . "http://settings-proxy:7890"))
    (cli-proxy . #f))
) ;define

;; provider 文件声明的 httpProxy 优先于 settings.httpProxy
(check (pi-cfg-provider-proxy CFG "kimi-coding")
  =>
  "http://provider-proxy:8080"
) ;check

;; 命令行 --proxy 优先于 provider 文件

(define CLI-CFG
  '((models-store ("kimi-coding" ("httpProxy" . "http://provider-proxy:8080")))
    (settings (http-proxy . "http://settings-proxy:7890"))
    (cli-proxy . "socks5h://127.0.0.1:1080"))
) ;define

(check (pi-cfg-provider-proxy CLI-CFG "kimi-coding")
  =>
  "socks5h://127.0.0.1:1080"
) ;check

;; provider 未声明代理时使用命令行代理
(check (pi-cfg-provider-proxy CLI-CFG "deepseek") => "socks5h://127.0.0.1:1080")

(check-report)
