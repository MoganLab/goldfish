(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; pi-resolve-proxy-url
;; 按照命令行参数 > 环境变量 > settings 顺序解析代理 URL。
(let ((settings '((http-proxy . "http://127.0.0.1:7890")))
      (clean-env '())
      (env-with-https '(("HTTPS_PROXY" . "https://corp-proxy:8080")))
      (env-with-all '(("ALL_PROXY" . "socks5h://127.0.0.1:1080"))))
  ;; CLI 选项优先
  (check (pi-resolve-proxy-url "socks5h://127.0.0.1:1080" settings clean-env)
    => "socks5h://127.0.0.1:1080")
  ;; HTTPS_PROXY 优先于 settings
  (check (pi-resolve-proxy-url #f settings env-with-https)
    => "https://corp-proxy:8080")
  ;; ALL_PROXY 优先于 settings
  (check (pi-resolve-proxy-url #f settings env-with-all)
    => "socks5h://127.0.0.1:1080")
  ;; 降级到 settings.httpProxy
  (check (pi-resolve-proxy-url #f settings clean-env)
    => "http://127.0.0.1:7890")
  (check (pi-resolve-proxy-url "" settings clean-env)
    => "http://127.0.0.1:7890")
  ;; 无代理直连
  (check (pi-resolve-proxy-url #f '((http-proxy . #f)) clean-env)
    => #f))

(check-report)
