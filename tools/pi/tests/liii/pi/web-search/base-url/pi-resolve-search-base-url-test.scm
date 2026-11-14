;; pi-resolve-search-base-url
;; 解析联网搜索使用的基础 URL。
;;
;; 语法
;; ----
;; (pi-resolve-search-base-url config opt-url)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist，可包含 provider 与 model-meta 字段。
;;
;; opt-url : (or string? boolean?)
;; 显式传入的基础 URL。
;;
;; 返回值
;; ----
;; string?
;; 生效的基础 URL 字符串，未配置时回退 Google 官方地址。

(import (scheme base)
  (liii check)
  (liii path)
  (liii pi ai config types)
  (liii pi web-search types)
  (liii pi web-search base-url)
) ;import

(check-set-mode! 'report-failed)

(let* ((empty-dir (path-join (path-temp-dir) "pi-web-search-empty-dir-base"))
       (clean-env `(("GOOGLE_BASE_URL" . "")
                    ("GEMINI_BASE_URL" . "")
                    ("GF_PI_BASE_URL" . "")
                    ("PI_CODING_AGENT_DIR" . ,(path->string empty-dir)))
       ) ;clean-env
      ) ;
  (path-mkdir empty-dir :parents #t :exist-ok #t)

  ;; 1. 显式传入的基础 URL 优先
  (check (pi-resolve-search-base-url '() "http://localhost:9999" #f "google" clean-env)
    =>
    "http://localhost:9999"
  ) ;check

  ;; 2. provider 为 google 时读取 model-meta 中的 base-url
  (check
    (pi-resolve-search-base-url '((provider . "google")
                                  (model-meta (base-url . "http://meta-base:8080")))
      #f
      #f
      "google"
      clean-env
    ) ;pi-resolve-search-base-url
    =>
    "http://meta-base:8080"
  ) ;check

  ;; 2b. model-meta 为 pi-model-metadata record 时，用访问器读取 base-url（issue #1172）
  (check
    (pi-resolve-search-base-url
      `((provider . "google")
        (model-meta . ,(%make-pi-model-metadata "gemini-2.5-flash"
                          "Gemini 2.5 Flash"
                          "google"
                          "http://record-meta-base:8080"
                          1048576
                          8192
                          #t
                        ) ;%make-pi-model-metadata
         ) ;model-meta
      ) ;config
      #f
      #f
      "google"
      clean-env
    ) ;pi-resolve-search-base-url
    =>
    "http://record-meta-base:8080"
  ) ;check

  ;; 2c. record 的 base-url 为空白字符串时视为未配置，回退默认地址（issue #1172）
  (check
    (pi-resolve-search-base-url
      `((provider . "google")
        (model-meta . ,(%make-pi-model-metadata "gemini-2.5-flash"
                          "Gemini 2.5 Flash"
                          "google"
                          "   "
                          1048576
                          8192
                          #t
                        ) ;%make-pi-model-metadata
         ) ;model-meta
      ) ;config
      #f
      #f
      "google"
      clean-env
    ) ;pi-resolve-search-base-url
    =>
    "https://generativelanguage.googleapis.com/v1beta"
  ) ;check

  ;; 3. web-search.json 覆盖
  (check
    (pi-resolve-search-base-url '()
      #f
      (make-pi-web-search-config "google" #f "http://ws-server:8080" #f)
      "google"
      clean-env
    ) ;pi-resolve-search-base-url
    =>
    "http://ws-server:8080"
  ) ;check

  ;; 4. 主会话是 kimi（无 google base-url），但 models-store 中 google 配了 baseUrl：复用 google baseUrl
  (let ((config
          `((provider . "kimi-coding")
            (models-store ("google"
                           ("name" . "Google")
                           ("baseUrl" . "http://google-store-server:8080"))
              ("kimi-coding" ("baseUrl" . "http://kimi-server:8080"))))
        ) ;config
       ) ;
    (check (pi-resolve-search-base-url config #f #f "google" clean-env)
      =>
      "http://google-store-server:8080"
    ) ;check
  ) ;let

  ;; 5. 未配置时回退默认地址
  (check (pi-resolve-search-base-url '() #f #f "google" clean-env)
    =>
    "https://generativelanguage.googleapis.com/v1beta"
  ) ;check

  ;; 6. 从 models/<provider>.json 读取 baseUrl
  (let* ((mock-dir (path-join (path-temp-dir) "pi-web-search-mock-base-models"))
         (mock-models (path-join mock-dir "models"))
         (mock-env `(("GOOGLE_BASE_URL" . "")
                     ("GEMINI_BASE_URL" . "")
                     ("GF_PI_BASE_URL" . "")
                     ("PI_CODING_AGENT_DIR" . ,(path->string mock-dir)))
         ) ;mock-env
        ) ;
    (path-mkdir mock-models :parents #t :exist-ok #t)
    (path-write-text (path->string (path-join mock-models "google.json"))
      "{\"baseUrl\": \"http://mock-google-server:8080\"}\n"
    ) ;path-write-text
    ;; 当会话不含 models-store 且无显式 url 时，直接读取 models/google.json 的 baseUrl
    (check (pi-resolve-search-base-url '() #f #f "google" mock-env)
      =>
      "http://mock-google-server:8080"
    ) ;check
  ) ;let*

) ;let*

(check-report)
