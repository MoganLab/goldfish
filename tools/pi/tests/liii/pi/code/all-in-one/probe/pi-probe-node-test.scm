;; pi-probe-node
;; 在后台脚本模式向候选节点发送微型 ping 预检。
;;
;; 语法
;; ----
;; (pi-probe-node node [timeout-ms config-dir run-fn])
;;
;; 参数
;; ----
;; node : <chain-node>
;; 待探测节点。
;; timeout-ms : number (可选)
;; 子进程超时毫秒数。
;; config-dir : (or string path #f) (可选)
;; 隔离配置目录，同时用于按天日志。
;; run-fn : procedure (可选)
;; 测试用运行器，接收 (command timeout-ms config-dir)，返回
;; (values exit-code stdout stderr)。
;;
;; 返回值
;; ----
;; boolean
;; 退出码为零且存在正常文本时返回 #t；否则记录 FAIL 并返回 #f。

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii either)
  (liii os)
  (liii subprocess)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code all-in-one probe)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-probe-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define NODE (make-chain-node "kimi1" "kimi-k3" "high"))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

(define (reset-log!)
  (let ((file (resolve-alarm-log-path TMP-DIR)))
    (when (file-exists? (path->string file))
      (delete-file (path->string file))
    ) ;when
  ) ;let
) ;define

(define (log-text)
  (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
) ;define

(define (check-failure status expected-code expected-text)
  (reset-log!)
  (check-false (pi-probe-node NODE
                 4321
                 TMP-DIR
                 (lambda (command timeout env) (values 1 "" status))
               ) ;pi-probe-node
  ) ;check-false
  (let ((content (log-text)))
    (check-true (string-contains? content " [FAIL] provider=kimi1 model=kimi-k3 "))
    (check-true (string-contains? content (string-append "error=" expected-code " "))
    ) ;check-true
    (check-true (string-contains? content expected-text))
  ) ;let
) ;define

;; 1. HTTP 200 等价于脚本进程正常退出；命令只携带 ping，不带主会话历史
(let ((captured #f))
  (check-true
    (pi-probe-node NODE
      4321
      TMP-DIR
      (lambda (command timeout env)
        (set! captured (list command timeout env))
        (values 0 "pong\n" "")
      ) ;lambda
    ) ;pi-probe-node
  ) ;check-true
  (let* ((command (car captured))
         (timeout (cadr captured))
         (config-dir (caddr captured))
        ) ;
    (check (car command) => (car (command-line)))
    (check (if (member "pi" command) #t #f) => #t)
    (check (if (member "-p" command) #t #f) => #t)
    (check (if (member "--no-session" command) #t #f) => #t)
    (check (if (member "--no-tools" command) #t #f) => #t)
    (check (if (member "--no-skills" command) #t #f) => #t)
    (check (if (member "--provider" command) #t #f) => #t)
    (check (if (member "kimi1" command) #t #f) => #t)
    (check (if (member "--model" command) #t #f) => #t)
    (check (if (member "kimi-k3" command) #t #f) => #t)
    (check (if (member "--thinking" command) #t #f) => #t)
    (check (if (member "high" command) #t #f) => #t)
    (check (car (reverse command)) => "ping")
    (check timeout => 4321)
    (check config-dir => TMP-DIR)
    (check (member "secret conversation" command) => #f)
  ) ;let*
) ;let

;; 2. 常见 HTTP 错误一视同仁，返回 #f 并追加对应错误码
(check-failure "Error: HTTP 529 after 3 attempts: overloaded"
  "529"
  "overloaded"
) ;check-failure
(check-failure "HTTP 429: exceeded_current_quota_error" "429" "quota")
(check-failure "HTTP 401: Invalid API Key" "401" "Invalid API Key")
(check-failure "HTTP 402: insufficient balance" "402" "balance")

;; 3. 超时、网络异常与空响应均不会把失败当作健康
(check-failure "probe timeout waiting for response" "timeout" "timeout")
(reset-log!)
(check-false (pi-probe-node NODE 10 TMP-DIR (lambda args (values -1 "" ""))))
(check-true (string-contains? (log-text) "error=timeout"))
(check-failure "connection refused" "error" "refused")
(reset-log!)
(check-false (pi-probe-node NODE 1000 TMP-DIR (lambda args (values 0 "" ""))))
(check-true (string-contains? (log-text) "error=empty-response"))

;; 4. 运行器自身抛错也必须收敛为 #f 和一条日志
(reset-log!)
(check-false (pi-probe-node NODE
               1000
               TMP-DIR
               (lambda args (error 'io-error "cannot spawn probe"))
             ) ;pi-probe-node
) ;check-false
(check-true (string-contains? (log-text) "error=exception"))

;; 5. 默认运行器必须真实执行公开 subprocess API，而非仅验证 mock
(reset-log!)
(let ((captured #f))
  (check-true
    (pi-probe-node NODE
      2000
      TMP-DIR
      (lambda (command timeout config-dir)
        (set! captured command)
        (let ((result (run-either '(printf "pong") :stdout 'capture :stderr 'capture :timeout 2)
              ) ;result
             ) ;
          (values 0 (to-right result) "")
        ) ;let
      ) ;lambda
    ) ;pi-probe-node
  ) ;check-true
  (check (car captured) => (car (command-line)))
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
