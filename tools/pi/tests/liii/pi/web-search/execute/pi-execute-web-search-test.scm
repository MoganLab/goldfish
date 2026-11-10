;; pi-execute-web-search
;; 执行联网搜索工具调用，校验参数、鉴权并返回搜索结果。
;;
;; 语法
;; ----
;; (pi-execute-web-search args . opts)
;;
;; 参数
;; ----
;; args : list?
;; web_search 工具参数 alist。
;;
;; opts : list?
;; 可选关键字参数，支持 :api-key、:provider、:config 等。
;;
;; 返回值
;; ----
;; list?
;; 包含 text、content 与 sources 字段的搜索结果 alist。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii os)
  (liii sys)
  (liii subprocess)
  (liii pi code core extensions)
  (liii pi web-search execute)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

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
               :provider "google"
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
               :provider "google"
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
      :provider "google"
      :base-url base-url
      :model-id "web-search-401"
      :api-key  "bad-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 6. HTTP 403 权限拒绝/超限测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :provider "google"
      :base-url base-url
      :model-id "web-search-403"
      :api-key  "test-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 7. HTTP 500 服务端错误测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :provider "google"
      :base-url base-url
      :model-id "error-500"
      :api-key  "test-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 8. 非 JSON 响应测试
  (check-catch 'error
    (pi-execute-web-search '(("query" . "scheme"))
      :provider "google"
      :base-url base-url
      :model-id "web-search-non-json"
      :api-key  "test-key"
      :timeout  10
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 9. 空 candidates 安全降级测试
  (let ((res (pi-execute-web-search '(("query" . "empty query"))
               :provider "google"
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
      :provider "google"
      :base-url base-url
      :model-id "web-search-hang"
      :api-key  "test-key"
      :timeout  1
    ) ;pi-execute-web-search
  ) ;check-catch

  ;; 11. 通过 execute-tool-call 分发执行（web_search 为扩展工具，需在注册表内分发）
  (let* ((api (make-extension-api))
         (_
           (pi-load-extension (cond ((file-exists? "tools/pi/extensions/web-search.scm")
                                     "tools/pi/extensions/web-search.scm"
                                    ) ;
                                    (else "extensions/web-search.scm")
                              ) ;cond
             api
           ) ;pi-load-extension
         ) ;_
         (call-res
           (with-extension-registry api
             (lambda ()
               (execute-tool-call "web_search"
                 '(("query" . "goldfish scheme"))
                 :provider "google"
                 :base-url base-url
                 :model-id "web-search-basic"
                 :api-key  "test-google-key"
                 :timeout  10
               ) ;execute-tool-call
             ) ;lambda
           ) ;with-extension-registry
         ) ;call-res
        ) ;
    (check-false (cdr (assoc 'isError call-res)))
    (let ((txt (cdr (assoc 'text call-res))))
      (check-true (string-contains? txt "Goldfish Scheme"))
    ) ;let
  ) ;let*

  ;; 关闭 stub server
  (process-kill proc)
  (process-wait proc)
) ;let*

(check-report)
