(import (scheme base)
        (liii check)
        (liii string)
        (liii subprocess)
        (liii pi code core extensions)
        (liii pi web-search execute)
        (liii pi web-search config)
        (liii pi code core tools))

(check-set-mode! 'report-failed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; e2e 测试（stub server）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-web-search-fixture)
  (let loop ((candidates '("tools/pi/tests/fixtures/web-search.json"
                           "tests/fixtures/web-search.json"
                           "fixtures/web-search.json"
                           "../fixtures/web-search.json"
                           "../../fixtures/web-search.json"
                           "../../../fixtures/web-search.json"
                           "../../../../tools/pi/tests/fixtures/web-search.json")))
    (cond ((null? candidates) (error 'file-not-found "web-search.json fixture not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates))))))

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
  ;; 注意：web-search.json 是全局覆盖，优先级高于 config 的 provider，
  ;;      因此用 :env 指向不存在的路径隔离开发者本机的真实全局配置
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
                                     :config '((provider . "deepseek") (api-key . "ds-test-key"))
                                     :env (list (cons "PI_WEB_SEARCH_CONFIG" "/nonexistent-gf-test/ws.json")
                                            (cons "HOME" "/nonexistent-gf-test")
                                            (cons "PI_CODING_AGENT_DIR" "/nonexistent-gf-test"))
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "Goldfish Official"))))

  ;; 5.2b web-search.json 全局覆盖：会话 provider 是别的，也走配置的 deepseek
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
                                     :config '((provider . "kimi-coding"))
                                     :env (list (cons "PI_WEB_SEARCH_CONFIG"
                                                  (find-web-search-fixture))
                                            (cons "HOME" "/nonexistent-gf-test")
                                            (cons "PI_CODING_AGENT_DIR" "/nonexistent-gf-test"))
                                     :api-key "ds-test-key"
                                     :base-url base-url
                                     :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "Goldfish Official"))))


  ;; 5.3 不带 :provider 且无任何配置时仍走 google 分支（google 场景已由
  ;;     pi-web-search-execute-test.scm 覆盖，这里验证 provider 默认值即可）
  (check (pi-resolve-web-search-provider '() #f #f) => "google")

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

  ;; 5.9 通过 execute-tool-call 分发（web_search 为扩展工具，需在注册表内分发）
  (let* ((api (make-extension-api))
         (_ (pi-load-extension
              (cond
                ((file-exists? "tools/pi/extensions/web-search.scm")
                 "tools/pi/extensions/web-search.scm")
                (else "extensions/web-search.scm")
              ) ;cond
              api
            ) ;pi-load-extension
         )
         (call-res
           (with-extension-registry api
             (lambda ()
               (execute-tool-call "web_search"
                 '(("query" . "goldfish scheme"))
                 :provider "deepseek"
                 :api-key "ds-test-key"
                 :base-url base-url
                 :timeout 10)))))
    (check-false (cdr (assoc 'isError call-res)))
    (check-true (string-contains? (cdr (assoc 'text call-res)) "Goldfish Official")))

  (process-kill proc)
  (process-wait proc))

(check-report)
