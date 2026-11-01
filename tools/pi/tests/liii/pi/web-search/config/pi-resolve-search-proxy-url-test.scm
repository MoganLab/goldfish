(import (liii check)
  (liii path)
  (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-search-proxy-url
;; 解析用于 web_search 联网搜索的 HTTP/HTTPS 代理 URL。
;;
;; 语法
;; ----
;; (pi-resolve-search-proxy-url config opt-proxy)
;; (pi-resolve-search-proxy-url config opt-proxy ws-config)
;; (pi-resolve-search-proxy-url config opt-proxy ws-config provider)
;; (pi-resolve-search-proxy-url config opt-proxy ws-config provider opt-env)
;;
;; 参数
;; ----
;; config : list?
;; 当前主会话的配置 alist，可包含 provider、proxy-url、models-store 等字段。
;;
;; opt-proxy : (or string? boolean?)
;; 显式传入的代理 URL，例如 CLI 参数 "--proxy" 或调用选项 ":proxy-url"。传入非空字符串时优先级最高。
;;
;; ws-config : (or list? boolean?)（可选）
;; web-search.json 解析后的配置 alist，可包含 httpProxy、proxy 或 http-proxy 字段。
;;
;; provider : (or string? boolean?)（可选）
;; 当前 web_search 使用的目标后端（如 "google" 或 "deepseek"），缺省时优先取会话 provider，回退 "google"。
;;
;; opt-env : (or list? boolean?)（可选）
;; 环境变量 alist（测试可控）；为 #f 或未提供时直接查询系统环境变量。
;;
;; 返回值
;; ----
;; (or string? #f)
;; 生效的代理 URL 字符串（如 "http://127.0.0.1:7890"）；若无需代理或无配置则返回 #f。
;;
;; 决策优先级
;; ----
;; 1. 显式 opt-proxy 字符串
;; 2. web-search.json 中的 httpProxy / proxy 字段
;; 3. 会话自身配置（若未显式指定 provider 且会话带 proxy-url，或会话 provider 与搜索一致）
;; 4. 搜索 provider 本地的代理配置：
;;    a. 会话 models-store 中对应 provider 的 httpProxy
;;    b. <config-dir>/models/<provider>.json 文件中的 httpProxy
;; 5. 环境变量 HTTPS_PROXY
;; 6. 环境变量 ALL_PROXY
;; 7. 全局 settings.json 中的 httpProxy
;; 8. 直连（返回 #f）
;;
;; 示例
;; ----
;; (pi-resolve-search-proxy-url '() "http://127.0.0.1:7890")
;; ;=> "http://127.0.0.1:7890"
;;
;; (pi-resolve-search-proxy-url '() #f '(("httpProxy" . "http://127.0.0.1:7890")))
;; ;=> "http://127.0.0.1:7890"

(let* ((empty-dir (path-join (path-temp-dir) "pi-web-search-empty-dir"))
       (clean-env `(("HTTPS_PROXY" . "")
                    ("ALL_PROXY" . "")
                    ("PI_CODING_AGENT_DIR" . ,(path->string empty-dir)))))
  (path-mkdir empty-dir :parents #t :exist-ok #t)

  ;; 1. 显式 opt-proxy 优先
  (check (pi-resolve-search-proxy-url '() "http://explicit:8080" #f "google" clean-env)
    => "http://explicit:8080"
  ) ;check

  ;; 2. ws-config (web-search.json) 覆盖
  (check (pi-resolve-search-proxy-url '() #f '(("httpProxy" . "http://ws-proxy:7890")) "google" clean-env)
    => "http://ws-proxy:7890"
  ) ;check
  (check (pi-resolve-search-proxy-url '() #f '(("proxy" . "http://ws-proxy-alias:7890")) "google" clean-env)
    => "http://ws-proxy-alias:7890"
  ) ;check

  ;; 3. 会话 provider 与 search provider 一致时的 proxy-url
  (check (pi-resolve-search-proxy-url
           '((provider . "google") (proxy-url . "http://session-google:7890"))
           #f #f "google" clean-env
         ) ;pi-resolve-search-proxy-url
    => "http://session-google:7890"
  ) ;check

  ;; 4. 主会话是 kimi（无代理），但 models-store 中 google 配了代理：自动复用 google 代理
  (let ((config `((provider . "kimi-coding")
                  (proxy-url . #f)
                  (models-store . (("google" ("httpProxy" . "http://google-store-proxy:7890"))
                                   ("kimi-coding" ("httpProxy" . #f)))))))
    (check (pi-resolve-search-proxy-url config #f #f "google" clean-env)
      => "http://google-store-proxy:7890"
    ) ;check
  ) ;let

  ;; 5. 环境变量 HTTPS_PROXY / ALL_PROXY
  (check (pi-resolve-search-proxy-url '() #f #f "google"
           `(("HTTPS_PROXY" . "http://env-https:8080")
             ("PI_CODING_AGENT_DIR" . ,(path->string empty-dir))))
    => "http://env-https:8080"
  ) ;check
  (check (pi-resolve-search-proxy-url '() #f #f "google"
           `(("ALL_PROXY" . "socks5h://env-all:1080")
             ("PI_CODING_AGENT_DIR" . ,(path->string empty-dir))))
    => "socks5h://env-all:1080"
  ) ;check

  ;; 6. 无任何代理时为 #f
  (check (pi-resolve-search-proxy-url '() #f #f "google" clean-env) => #f)

  ;; 7. 旧签名兼容调用：(pi-resolve-search-proxy-url config opt-proxy)
  (check (pi-resolve-search-proxy-url '() "http://direct:7890") => "http://direct:7890")
  (check (pi-resolve-search-proxy-url '((proxy-url . "http://cfg-proxy:7890")) #f)
    => "http://cfg-proxy:7890"
  ) ;check

  ;; 8. proxy alist 转换
  (check (pi-resolve-search-proxy-alist "http://127.0.0.1:7890")
    => '(("http" . "http://127.0.0.1:7890") ("https" . "http://127.0.0.1:7890"))
  ) ;check
  (check (pi-resolve-search-proxy-alist #f) => '())
  (check (pi-resolve-search-proxy-alist "") => '())

  ;; 9. 从 models/<provider>.json 读取 proxy
  (let* ((mock-dir (path-join (path-temp-dir) "pi-web-search-mock-models"))
         (mock-models (path-join mock-dir "models"))
         (mock-env `(("HTTPS_PROXY" . "")
                     ("ALL_PROXY" . "")
                     ("PI_CODING_AGENT_DIR" . ,(path->string mock-dir)))))
    (path-mkdir mock-models :parents #t :exist-ok #t)
    (path-write-text (path->string (path-join mock-models "google.json"))
      "{\"httpProxy\": \"http://mock-google-proxy:7890\"}\n"
    ) ;path-write-text
    ;; 当会话不含 models-store 且无 proxy-url 时，直接读取 models/google.json 的 httpProxy
    (check (pi-resolve-search-proxy-url '() #f #f "google" mock-env)
      => "http://mock-google-proxy:7890"
    ) ;check
  ) ;let

) ;let

(check-report)
