;; pi-chain-execute
;; 每次从链头顺次预检，由首个健康节点执行真实业务请求。
;;
;; 语法
;; ----
;; (pi-chain-execute chain-def messages user-fn cfg
;;                   [config-dir run-fn probe-fn])
;;
;; 参数
;; ----
;; chain-def : <chain-def>
;; 按原始顺序探测的链定义。
;; messages : list
;; 原始会话消息列表。
;; user-fn : procedure
;; 接收 provider、model、thinking-level、clean-messages 的真实业务函数。
;; cfg : <all-in-one-cfg>
;; 提供预检超时毫秒数。
;; config-dir、run-fn、probe-fn : 可选
;; 隔离目录、子进程运行器与预检函数，后两者用于测试注入。
;;
;; 返回值
;; ----
;; any
;; 返回 user-fn 结果；所有节点失败时返回 all-nodes-exhausted。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi ai message)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-chain-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define N1 (make-chain-node "kimi1" "k3" "high"))

(define N2 (make-chain-node "deepseek1" "chat" "medium"))

(define N3 (make-chain-node "google1" "gemini" "low"))

(define CHAIN (make-chain-def "coding-deep" "深度链" (list N1 N2 N3)))

(define CFG (make-all-in-one-cfg #f 1000 10 3456 (list CHAIN)))

(define USER (pi-user-message "continue"))

(define BROKEN
  (pi-assistant-message (list (pi-text-block "dirty")) "error" "HTTP 529")
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
(pi-chain-reset-rewind-state!)

;; 1. 节点 1 故障时顺次落到节点 2；只执行一次真实业务，且上下文已清洗
(let ((healthy '("kimi2" "deepseek1")) (probed '()) (calls '()))
  (check
    (pi-chain-execute CHAIN
      (list USER BROKEN)
      (lambda (provider model level messages)
        (set! calls (cons (list provider model level messages) calls))
        'executed-on-node-2
      ) ;lambda
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir run-fn)
        (set! probed (cons (chain-node-provider node) probed))
        (check timeout-ms => 3456)
        (check config-dir => TMP-DIR)
        (check run-fn => #f)
        (member (chain-node-provider node) healthy)
      ) ;lambda
    ) ;pi-chain-execute
    =>
    'executed-on-node-2
  ) ;check
  (check (reverse probed) => '("kimi1" "deepseek1"))
  (check (length calls) => 1)
  (let ((call (car calls)))
    (check (list-ref call 0) => "deepseek1")
    (check (list-ref call 1) => "chat")
    (check (list-ref call 2) => "medium")
    (check (list-ref call 3) => (list USER))
  ) ;let
  (check-true
    (string-contains? (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
      "[SWITCH]"
    ) ;string-contains?
  ) ;check-true
) ;let

;; 2. 节点 1 恢复后，下一次调用仍从链头开始并自动回弹
(let ((probed '()) (executed #f))
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda (provider model level messages)
        (set! executed provider)
        'executed-on-node-1
      ) ;lambda
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir run-fn)
        (set! probed (cons (chain-node-provider node) probed))
        #t
      ) ;lambda
    ) ;pi-chain-execute
    =>
    'executed-on-node-1
  ) ;check
  (check (reverse probed) => '("kimi1"))
  (check executed => "kimi1")
  (check-true
    (string-contains? (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
      "[REWIND]"
    ) ;string-contains?
  ) ;check-true
) ;let

;; 3. 三节点全部失败时不调用 user-fn，返回统一枯竭符号
(let ((probed '()) (called? #f))
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda args (set! called? #t) 'unexpected)
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir run-fn)
        (set! probed (cons (chain-node-provider node) probed))
        #f
      ) ;lambda
    ) ;pi-chain-execute
    =>
    'all-nodes-exhausted
  ) ;check
  (check (reverse probed) => '("kimi1" "deepseek1" "google1"))
  (check-false called?)
  (check-true
    (string-contains? (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
      "[EXHAUSTED]"
    ) ;string-contains?
  ) ;check-true
) ;let

;; 4. 空链直接枯竭；参数类型错误明确暴露
(let ((empty (make-chain-def "empty" "空链" '())))
  (check (pi-chain-execute empty '() (lambda args 'bad) CFG TMP-DIR #f (lambda args #t))
    =>
    'all-nodes-exhausted
  ) ;check
) ;let
(check-catch 'type-error (pi-chain-execute #f '() (lambda args #t) CFG))
(check-catch 'type-error (pi-chain-execute CHAIN '() (lambda args #t) #f))
(check-catch 'type-error (pi-chain-execute CHAIN '() #f CFG))
(check-catch 'type-error (pi-chain-execute CHAIN #f (lambda args #t) CFG))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
