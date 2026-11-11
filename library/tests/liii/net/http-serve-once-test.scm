;; http-serve-once
;; 启动单次 HTTP 监听器并捕获回调请求中的授权码。
;;
;; 语法
;; ----
;; (http-serve-once [callback-path] [handler])
;;
;; 参数
;; ----
;; callback-path : string? (可选，默认 "/callback")
;; HTTP 回调路径。
;; handler : procedure? (可选)
;; 接收原始 HTTP 请求字符串的单参过程，其返回值作为本函数结果；
;; 未提供时默认从查询参数中提取 "code"。
;;
;; 返回值
;; -----
;; any
;; 默认返回从 HTTP GET 请求查询参数中提取的 "code" 字符串（未携带时为空字符串）；
;; 提供 handler 时返回 handler 的调用结果。
;;
;; 错误处理
;; ----
;; type-error
;; 当 callback-path 不是 string 或 handler 不是 procedure 时抛出。
;; net-error
;; 当底层监听或 accept 失败时抛出。

(import (scheme base)
        (liii check)
        (liii string)
        (liii sys)
        (liii subprocess)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：后台启动服务进程，主进程模拟浏览器回调
(let* ((server-code
         "(import (liii net)) (display (http-serve-once \"/callback\"))")
       (p (process-spawn
            (list (executable) "eval" server-code)
            :stdout 'pipe
            :stderr 'pipe)))
  ;; 轮询服务进程 stdout，等待 “Listening on ...” 行
  (define (read-until-listening proc acc)
    (let ((line (process-read-line proc)))
      (cond ((eof-object? line) #f)
            ((string-contains? line "Listening on ")
             (let* ((url-start (string-position "http://" line))
                    (rest (substring line url-start (string-length line)))
                    (slash-idx (string-position "/" rest 7))
                    (host-port (substring rest 7 (if slash-idx slash-idx (string-length rest)))))
               host-port))
            (else (read-until-listening proc (string-append acc line)))
      ) ;cond
    ) ;let
  ) ;define

  (let ((host-port (read-until-listening p "")))
    (check-true (string? host-port))
    ;; 模拟浏览器重定向：直接向回调地址发起带 code 的 GET 请求
    (let* ((client (net-dial 'tcp host-port))
           (req (string-append "GET /callback?code=mock_oauth_secret_888 HTTP/1.1\r\n"
                   "Host: " host-port "\r\n"
                   "Connection: close\r\n\r\n"))
           (n (net-conn-write client req))
           (resp (net-conn-read client 4096))
           (resp-str (if (bytevector? resp) (utf8->string resp) "")))
      (check-true (> n 0))
      (check-true (string-contains? resp-str "200 OK"))
      (net-conn-close client))
    ;; 服务进程应提取出 code 并正常退出
    (check (process-wait p) => 0)
    (process-close p)))

;; 测试项2：自定义 handler 接收原始请求并返回自定义结果
(let* ((server-code
         "(import (liii net)) (display (http-serve-once \"/cb\" (lambda (req) (if (zero? (string-length req)) \"EMPTY\" \"GOT-REQUEST\"))))")
       (p (process-spawn
            (list (executable) "eval" server-code)
            :stdout 'pipe
            :stderr 'pipe)))
  (define (read-until-listening proc)
    (let ((line (process-read-line proc)))
      (cond ((eof-object? line) #f)
            ((string-contains? line "Listening on ")
             (let* ((url-start (string-position "http://" line))
                    (rest (substring line url-start (string-length line)))
                    (slash-idx (string-position "/" rest 7))
                    (host-port (substring rest 7 (if slash-idx slash-idx (string-length rest)))))
               host-port))
            (else (read-until-listening proc))
      ) ;cond
    ) ;let
  ) ;define

  (let ((host-port (read-until-listening p)))
    (check-true (string? host-port))
    (let* ((client (net-dial 'tcp host-port))
           (req (string-append "GET /cb?token=abc123 HTTP/1.1\r\n"
                   "Host: " host-port "\r\n"
                   "Connection: close\r\n\r\n"))
           (n (net-conn-write client req))
           (resp (net-conn-read client 4096)))
      (check-true (> n 0))
      (check-true (bytevector? resp))
      (net-conn-close client))
    ;; 服务进程应以自定义 handler 的结果退出
    (check (process-wait p) => 0)
    (let ((out (process-read-line p)))
      (check-true (string? out))
      (check-true (string-contains? out "GOT-REQUEST")))
    (process-close p)))

;; 测试项3：参数类型校验
(check-catch 'type-error (http-serve-once 123))
(check-catch 'type-error (http-serve-once "/callback" "not-a-proc"))

(check-report)
