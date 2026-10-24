(import (scheme base)
        (liii check)
        (liii string)
        (liii json)
        (liii subprocess)
        (liii pi code tools web-search)
        (liii pi code tools web-search config)
        (liii pi code tools web-search deepseek)
        (liii pi code tools))

(check-set-mode! 'report-failed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. provider 解析测试
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 显式 opt 优先
(check (pi-resolve-web-search-provider '() "deepseek") => "deepseek")
(check (pi-resolve-web-search-provider '((provider . "deepseek")) "google") => "google")
;; config 的 web-search-provider 字段
(check (pi-resolve-web-search-provider '((web-search-provider . "deepseek")) #f) => "deepseek")
;; config 的 provider 字段
(check (pi-resolve-web-search-provider '((provider . "deepseek")) #f) => "deepseek")
;; 环境变量
(check (pi-resolve-web-search-provider '() #f '(("GF_PI_WEB_SEARCH_PROVIDER" . "deepseek"))) => "deepseek")
;; 默认 google
(check (pi-resolve-web-search-provider '() #f) => "google")
(check (pi-resolve-web-search-provider '((provider . "anthropic")) #f) => "google")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. deepseek key 解析测试
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check (pi-resolve-deepseek-api-key '() "opt-key") => "opt-key")
(check (pi-resolve-deepseek-api-key '() "  ") => #f) ; 空白 opt 视为未设置
(check (pi-resolve-deepseek-api-key '((provider . "deepseek") (api-key . "cfg-key")) #f) => "cfg-key")
;; provider 非 deepseek 时不能用 config 的 api-key（那是别家的 key）
(check (pi-resolve-deepseek-api-key '((provider . "google") (api-key . "g-key")) #f
              '(("HOME" . "/nonexistent-gf-test")
                ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))) => #f)
;; 环境变量
(check (pi-resolve-deepseek-api-key '() #f '(("DEEPSEEK_API_KEY" . "env-key")
                                              ("HOME" . "/nonexistent-gf-test")
                                              ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))) => "env-key")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. deepseek base-url 解析测试
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check (pi-resolve-deepseek-search-base-url '() #f) => "https://api.deepseek.com")
(check (pi-resolve-deepseek-search-base-url '() "http://localhost:9999") => "http://localhost:9999")
;; anthropic 兼容端点后缀要剥离
(check (pi-resolve-deepseek-search-base-url '() "https://api.deepseek.com/anthropic") => "https://api.deepseek.com")
(check (pi-resolve-deepseek-search-base-url '() #f '(("DEEPSEEK_BASE_URL" . "https://api.deepseek.com/anthropic"))) => "https://api.deepseek.com")
;; config 的 model-meta baseURL（provider 为 deepseek 时）
(check (pi-resolve-deepseek-search-base-url
         '((provider . "deepseek")
           (model-meta . (("baseURL" . "https://api.deepseek.com/anthropic")))) #f) => "https://api.deepseek.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. 请求构造与响应解析（纯函数）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check (deepseek-build-search-request-body "hello") => '(("query" . "hello") ("count" . 10)))

(let* ((json-str "{\"query\":\"goldfish\",\"results\":[{\"title\":\"T1\",\"url\":\"https://u1\",\"snippet\":\"s1\",\"summary\":\"m1\"},{\"title\":\"T2\",\"url\":\"https://u2\",\"snippet\":\"s2\"}]}")
       (json-data (string->json json-str))
       (results (deepseek-parse-search-results json-data)))
  (check (length results) => 2)
  (check (cdr (assoc "title" (car results))) => "T1")
  ;; 无 summary 时回退到 snippet
  (check (cdr (assoc "summary" (cadr results))) => "s2")
  ;; sources 提取
  (let ((sources (deepseek-sources-from-results results)))
    (check (length sources) => 2)
    (check (cdr (assoc 'title (car sources))) => "T1")
    (check (cdr (assoc 'url (car sources))) => "https://u1"))
  ;; 正文拼接
  (let ((text (deepseek-format-search-text results)))
    (check-true (string-contains? text "T1"))
    (check-true (string-contains? text "m1"))))

;; 空 results
(check (deepseek-parse-search-results (string->json "{\"results\":[]}")) => '())
(check (deepseek-parse-search-results (string->json "{}")) => '())

;; fetch 页面解析
(let ((page (deepseek-parse-fetch-page
              (string->json "{\"title\":\"Example Page\",\"content\":\"# Example\\nbody\"}"))))
  (check (cdr (assoc "title" page)) => "Example Page")
  (check (cdr (assoc "content" page)) => "# Example\nbody"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. e2e 测试（stub server）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-stub-server)
  (let loop ((candidates '("tools/pi/tests/stub_deepseek_server.py"
                           "tests/stub_deepseek_server.py"
                           "stub_deepseek_server.py"
                           "../stub_deepseek_server.py"
                           "../../stub_deepseek_server.py"
                           "../../../stub_deepseek_server.py"
                           "../../../../tools/pi/tests/stub_deepseek_server.py"
                           "../../../../../tools/pi/tests/stub_deepseek_server.py")))
    (cond ((null? candidates) (error 'file-not-found "stub_deepseek_server.py not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates))))))

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (substring line 5 (string-length line)))
    #f))

(let* ((proc (process-spawn (list "python3" (find-stub-server)) :stdout 'pipe))
       (port (parse-port-line (process-read-line proc)))
       (base-url (string-append "http://127.0.0.1:" (number->string port))))
  (check-true (number? port))

  ;; 5.1 正常搜索：显式 :provider "deepseek"
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
                                     :provider "deepseek"
                                     :api-key "ds-test-key"
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "Goldfish Official"))
      (check-true (string-contains? txt "https://goldfish.liii.pro"))
      (check-true (string-contains? txt "## Sources")))
    (check (length (cdr (assoc 'sources res))) => 2))

  ;; 5.2 通过 config 的 provider 自动分发（不带 :provider opt）
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
                                     :config '((provider . "deepseek") (api-key . "ds-test-key"))
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "Goldfish Official"))))

  ;; 5.3 不带 :provider 且 config 为空时仍走 google 分支（google 场景已由
  ;;     pi-web-search-execute-test.scm 覆盖，这里验证 provider 默认值即可）
  (check (pi-resolve-web-search-provider '() #f) => "google")

  ;; 5.4 带 urls：deepseek 分支用 fetch API 读取页面
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme")
                                      ("urls" . ("https://example.com/page1")))
                                     :provider "deepseek"
                                     :api-key "ds-test-key"
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (check-true (string-contains? (cdr (assoc 'text res)) "Example Page")))

  ;; 5.5 HTTP 401 鉴权失败
  (check-catch 'error
    (pi-execute-web-search '(("query" . "goldfish scheme"))
                           :provider "deepseek"
                           :api-key "bad-key"
                           :base-url base-url
                           :timeout 10))

  ;; 5.6 HTTP 500 服务端错误
  (check-catch 'error
    (pi-execute-web-search '(("query" . "error-500"))
                           :provider "deepseek"
                           :api-key "ds-test-key"
                           :base-url base-url
                           :timeout 10))

  ;; 5.7 空结果
  (let ((res (pi-execute-web-search '(("query" . "empty results"))
                                     :provider "deepseek"
                                     :api-key "ds-test-key"
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (check-true (string-contains? (cdr (assoc 'text res)) "No search results")))

  ;; 5.8 超时
  (check-catch 'error
    (pi-execute-web-search '(("query" . "slow search"))
                           :provider "deepseek"
                           :api-key "ds-test-key"
                           :base-url base-url
                           :timeout 1))

  ;; 5.9 通过 execute-tool-call 分发
  (let ((call-res (execute-tool-call "web_search"
                                     '(("query" . "goldfish scheme"))
                                     :provider "deepseek"
                                     :api-key "ds-test-key"
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError call-res)))
    (check-true (string-contains? (cdr (assoc 'text call-res)) "Goldfish Official")))

  (process-kill proc)
  (process-wait proc))

(check-report)
