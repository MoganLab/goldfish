(import (scheme base)
        (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
        (liii sys)
        (liii subprocess)
        (liii pi code core extensions)
        (liii pi web-search execute)
        (liii pi code core tools))

(check-set-mode! 'report-failed)

;; 1. 参数校验测试
(check-catch 'type-error (pi-execute-web-search '()))
(check-catch 'type-error (pi-execute-web-search '(("query" . 123))))
(check-catch 'value-error (pi-execute-web-search '(("query" . "   "))))
(check-catch 'type-error (pi-execute-web-search '(("query" . "scheme") ("urls" . "not-a-list"))))
(check-catch 'type-error (pi-execute-web-search '(("query" . "scheme") ("urls" . (123)))))

;; 2. 鉴权缺失测试（显式指定空 key）
(check-catch 'error
  (pi-execute-web-search '(("query" . "scheme")) :api-key ""))

;; 启动 stub server
(define (find-stub-server)
  (let loop ((candidates '("tools/pi/tests/stub_gemini_server.py"
                           "tests/stub_gemini_server.py"
                           "stub_gemini_server.py"
                           "../stub_gemini_server.py"
                           "../../stub_gemini_server.py"
                           "../../../stub_gemini_server.py"
                           "../../../../tools/pi/tests/stub_gemini_server.py"
                           "../../../../../tools/pi/tests/stub_gemini_server.py")))
    (cond ((null? candidates) (error 'file-not-found "stub_gemini_server.py not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates))))))

(define stub-server-file (find-stub-server))

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (substring line 5 (string-length line)))
    #f))

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port))))
  (check-true (number? port))

  ;; 3. 正常搜索请求测试
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
                                   :provider "google"
                                   :base-url base-url
                                   :model-id "web-search-basic"
                                   :api-key "test-google-key"
                                   :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "Goldfish Scheme is a minimalist"))
      (check-true (string-contains? txt "## Sources"))
      (check-true (string-contains? txt "Goldfish Official - https://goldfish.liii.pro")))
    (check (length (cdr (assoc 'sources res))) => 1))

  ;; 4. 带 URLs 的调用测试
  (let ((res (pi-execute-web-search '(("query" . "compare goldfish")
                                      ("urls" . ("https://goldfish.liii.pro" "https://scheme.org")))
                                   :provider "google"
                                   :base-url base-url
                                   :model-id "web-search-urls"
                                   :api-key "test-google-key"
                                   :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "prompt_has_urls=True"))))

  ;; 5. HTTP 401 鉴权失败测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
                           :provider "google"
                           :base-url base-url
                           :model-id "web-search-401"
                           :api-key "bad-key"
                           :timeout 10))

  ;; 6. HTTP 403 权限拒绝/超限测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
                           :provider "google"
                           :base-url base-url
                           :model-id "web-search-403"
                           :api-key "test-key"
                           :timeout 10))

  ;; 7. HTTP 500 服务端错误测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
                           :provider "google"
                           :base-url base-url
                           :model-id "error-500"
                           :api-key "test-key"
                           :timeout 10))

  ;; 8. 非 JSON 响应测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
                           :provider "google"
                           :base-url base-url
                           :model-id "web-search-non-json"
                           :api-key "test-key"
                           :timeout 10))

  ;; 9. 空 candidates 安全降级测试
  (let ((res (pi-execute-web-search '(("query" . "empty query"))
                                   :provider "google"
                                   :base-url base-url
                                   :model-id "web-search-empty-candidates"
                                   :api-key "test-key"
                                   :timeout 10)))
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "No search results or candidates returned"))))

  ;; 10. 超时控制测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "slow query"))
                           :provider "google"
                           :base-url base-url
                           :model-id "web-search-hang"
                           :api-key "test-key"
                           :timeout 1))

  ;; 11. 通过 execute-tool-call 分发执行（web_search 为扩展工具，需在注册表内分发）
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
                 :provider "google"
                 :base-url base-url
                 :model-id "web-search-basic"
                 :api-key "test-google-key"
                 :timeout 10)))))
    (check-false (cdr (assoc 'isError call-res)))
    (let ((txt (cdr (assoc 'text call-res))))
      (check-true (string-contains? txt "Goldfish Scheme"))))

  ;; 关闭 stub server
  (process-kill proc)
  (process-wait proc))

(check-report)
