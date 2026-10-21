(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii os)
  (liii sys)
  (liii subprocess)
  (liii pi web-search)
  (liii pi tools)
) ;import

(check-set-mode! 'report-failed)

;; 辅助环境隔离过程

(define (with-test-env vars-alist thunk)
  (let ((saved-vars
          (map
            (lambda (p) (cons (car p) (get-environment-variable (car p))))
            vars-alist
          ) ;map
        ) ;saved-vars
       ) ;
    (dynamic-wind
      (lambda ()
        (for-each
          (lambda (p) (if (cdr p) (putenv (car p) (cdr p)) (unsetenv (car p))))
          vars-alist
        ) ;for-each
      ) ;lambda
      thunk
      (lambda ()
        (for-each
          (lambda (p) (if (cdr p) (putenv (car p) (cdr p)) (unsetenv (car p))))
          saved-vars
        ) ;for-each
      ) ;lambda
    ) ;dynamic-wind
  ) ;let
) ;define

;; 1. 参数校验测试
(check-catch 'type-error (pi-execute-web-search '()))
(check-catch 'type-error (pi-execute-web-search '(("query" . 123))))
(check-catch 'value-error (pi-execute-web-search '(("query" . "   "))))
(check-catch 'type-error
  (pi-execute-web-search '(("query" . "scheme") ("urls" . "not-a-list")))
) ;check-catch
(check-catch 'type-error
  (pi-execute-web-search '(("query" . "scheme") ("urls" 123)))
) ;check-catch

;; 2. 鉴权缺失测试（显式指定空 key）
(check-catch 'error (pi-execute-web-search '(("query" . "scheme")) :api-key ""))

;; 启动 stub server

(define (find-stub-server)
  (let loop
    ((candidates '("tools/pi/tests/stub_gemini_server.py"
                   "tests/stub_gemini_server.py" "stub_gemini_server.py"
                   "../stub_gemini_server.py" "../../stub_gemini_server.py"
                   "../../../stub_gemini_server.py"
                   "../../../../tools/pi/tests/stub_gemini_server.py"
                   "../../../../../tools/pi/tests/stub_gemini_server.py")
     ) ;candidates
    ) ;
    (cond ((null? candidates) (error 'file-not-found "stub_gemini_server.py not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(define stub-server-file (find-stub-server))

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (substring line 5 (string-length line)))
    #f
  ) ;if
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))

  ;; 3. 正常搜索请求测试
  (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
               :base-url base-url
               :model-id "web-search-basic"
               :api-key  "test-google-key"
               :timeout  10
             ) ;pi-execute-web-search
        ) ;res
       ) ;
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "Goldfish Scheme is a minimalist"))
      (check-true (string-contains? txt "## Sources"))
      (check-true (string-contains? txt "Goldfish Official - https://goldfish.liii.pro")
      ) ;check-true
    ) ;let
    (check (length (cdr (assoc 'sources res))) => 1)
  ) ;let

  ;; 4. 带 URLs 的调用测试
  (let ((res (pi-execute-web-search '(("query" . "compare goldfish")
                                      ("urls"
                                       "https://goldfish.liii.pro"
                                       "https://scheme.org"))
               :base-url base-url
               :model-id "web-search-urls"
               :api-key  "test-google-key"
               :timeout  10
             ) ;pi-execute-web-search
        ) ;res
       ) ;
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "prompt_has_urls=True"))
    ) ;let
  ) ;let

  ;; 5. HTTP 401 鉴权失败测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :base-url base-url
      :model-id "web-search-401"
      :api-key  "bad-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 6. HTTP 403 权限拒绝/超限测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :base-url base-url
      :model-id "web-search-403"
      :api-key  "test-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 7. HTTP 500 服务端错误测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :base-url base-url
      :model-id "error-500"
      :api-key  "test-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 8. 非 JSON 响应测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :base-url base-url
      :model-id "web-search-non-json"
      :api-key  "test-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 9. 空 candidates 安全降级测试
  (let ((res (pi-execute-web-search '(("query" . "empty query"))
               :base-url base-url
               :model-id "web-search-empty-candidates"
               :api-key  "test-key"
               :timeout  10
             ) ;pi-execute-web-search
        ) ;res
       ) ;
    (check-false (cdr (assoc 'isError res)))
    (let ((txt (cdr (assoc 'text res))))
      (check-true (string-contains? txt "No search results or candidates returned"))
    ) ;let
  ) ;let

  ;; 10. 超时控制测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "slow query"))
      :base-url base-url
      :model-id "web-search-hang"
      :api-key  "test-key"
      :timeout  1
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 11. 通过 execute-tool-call 分发执行
  (let ((call-res (execute-tool-call "web_search"
                    '(("query" . "goldfish scheme"))
                    :base-url base-url
                    :model-id "web-search-basic"
                    :api-key  "test-google-key"
                    :timeout  10
                  ) ;execute-tool-call
        ) ;call-res
       ) ;
    (check-false (cdr (assoc 'isError call-res)))
    (let ((txt (cdr (assoc 'text call-res))))
      (check-true (string-contains? txt "Goldfish Scheme"))
    ) ;let
  ) ;let

  ;; 12. 凭据回退集成测试：config 中 api-key 为 #f 时正确回退到 GOOGLE_API_KEY 环境变量
  (with-test-env '(("GOOGLE_API_KEY" . "test-google-key")
                   ("GEMINI_API_KEY" . #f))
    (lambda ()
      (let ((res (pi-execute-web-search '(("query" . "goldfish scheme"))
                   :base-url base-url
                   :model-id "web-search-basic"
                   :config   (list (cons 'api-key #f))
                   :timeout  10
                 ) ;pi-execute-web-search
            ) ;res
           ) ;
        (check-false (cdr (assoc 'isError res)))
        (let ((txt (cdr (assoc 'text res))))
          (check-true (string-contains? txt "Goldfish Scheme"))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;with-test-env

  ;; 关闭 stub server
  (process-kill proc)
  (process-wait proc)
) ;let*

(check-report)
