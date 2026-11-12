;; pi-chain-execute
;; 粘性节点探活、冷却期防雪崩与优先自愈回溯。
;;
;; 语法
;; ----
;; (pi-chain-execute chain-def messages user-fn cfg
;;                   [config-dir request-fn probe-fn skip-index now-fn])
;;
;; 参数
;; ----
;; chain-def : <chain-def>
;; 包含候选节点的链定义。
;; messages : list
;; 原始会话消息列表。
;; user-fn : procedure
;; 接收 provider、model、thinking-level、clean-messages 的真实业务函数。
;; cfg : <all-in-one-cfg>
;; 提供预检超时与冷却窗口毫秒数（retryIntervalMs，缺省 60 秒）。
;; config-dir、request-fn、probe-fn、skip-index、now-fn : 可选
;; 隔离目录、单次请求函数、预检函数、跳过下标与时间注入（支持数值或无参过程）。
;;
;; 返回值
;; ----
;; any
;; 返回 user-fn 结果；所有节点失败或均在冷却期时返回 all-nodes-exhausted。

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

;; retryIntervalMs 为 60000 毫秒（60 秒）
(define CFG (make-all-in-one-cfg #f 1000 60000 3456 (list CHAIN)))

(define USER (pi-user-message "continue"))

(define BROKEN
  (pi-assistant-message (list (pi-text-block "dirty")) "error" "HTTP 529")
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
(pi-chain-reset-rewind-state!)

;; 1. 节点 1 在 T=1000 故障，顺次探活落到节点 2；记录节点 1 故障时间戳并在执行前清洗边界
(let ((healthy '("deepseek1")) (probed '()) (calls '()))
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
      (lambda (node timeout-ms config-dir request-fn)
        (set! probed (cons (chain-node-provider node) probed))
        (check timeout-ms => 3456)
        (check config-dir => TMP-DIR)
        (check request-fn => #f)
        (member (chain-node-provider node) healthy)
      ) ;lambda
      #f
      1000
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
  (check (pi-chain-node-last-failure N1) => 1000)
  (check-true
    (string-contains? (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
      "[SWITCH]"
    ) ;string-contains?
  ) ;check-true
) ;let

;; 2. 粘性节点机制：节点 2 可用时保持粘性，直接探活并正常使用节点 2，不探活节点 1
(let ((probed '()) (executed #f))
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda (provider model level messages)
        (set! executed provider)
        'executed-on-node-2-again
      ) ;lambda
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir request-fn)
        (set! probed (cons (chain-node-provider node) probed))
        #t
      ) ;lambda
      #f
      5000
    ) ;pi-chain-execute
    =>
    'executed-on-node-2-again
  ) ;check
  ;; 验证只探活了粘性节点 deepseek1，没有重新从链头 kimi1 探活
  (check (reverse probed) => '("deepseek1"))
  (check executed => "deepseek1")
) ;let

;; 3. 冷却期跳过机制：在 T=10000（距离 kimi1 故障仅 9 秒 < 60 秒冷却期），节点 2 故障；
;; kimi1 仍处于冷却期内被跳过，避免无效超时；顺延探测节点 3 并成功切换
(let ((probed '()) (executed #f))
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda (provider model level messages)
        (set! executed provider)
        'executed-on-node-3
      ) ;lambda
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir request-fn)
        (set! probed (cons (chain-node-provider node) probed))
        (equal? (chain-node-provider node) "google1")
      ) ;lambda
      #f
      10000
    ) ;pi-chain-execute
    =>
    'executed-on-node-3
  ) ;check
  ;; 粘性节点 deepseek1 先探活（失败）；kimi1 处于冷却期被跳过；顺延探测 google1（成功）
  (check (reverse probed) => '("deepseek1" "google1"))
  (check executed => "google1")
  (check (pi-chain-node-last-failure N2) => 10000)
) ;let

;; 4. 冷却期自愈回溯：在 T=70000（距离 kimi1 故障 69 秒 >= 60 秒冷却期），节点 3 故障；
;; kimi1 已超出冷却期，优先对 kimi1 重新探活；kimi1 恢复可用，自愈回溯至链头并记录 REWIND
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
      (lambda (node timeout-ms config-dir request-fn)
        (set! probed (cons (chain-node-provider node) probed))
        ;; 粘性节点 google1 故障；超出冷却期的链头 kimi1 恢复可用
        (equal? (chain-node-provider node) "kimi1")
      ) ;lambda
      #f
      70000
    ) ;pi-chain-execute
    =>
    'executed-on-node-1
  ) ;check
  ;; 先探活粘性节点 google1（失败）；kimi1 超出冷却期被优先探活（成功）
  (check (reverse probed) => '("google1" "kimi1"))
  (check executed => "kimi1")
  ;; 节点 1 自愈成功后故障标记已清除
  (check-false (pi-chain-node-last-failure N1))
  (check-true
    (string-contains? (path-read-text (path->string (resolve-alarm-log-path TMP-DIR)))
      "[REWIND]"
    ) ;string-contains?
  ) ;check-true
) ;let

;; 5. 冷却期探活失败刷新：在 T=135000（距离 deepseek1 故障 125s、google1 故障 65s 已过冷却），节点 1 故障；
;; 检查 deepseek1：超出冷却期进行探活但仍失败，刷新其故障时间戳为 135000 并顺延到 google1
(let ((probed '()) (executed #f))
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda (provider model level messages)
        (set! executed provider)
        'executed-on-node-3-again
      ) ;lambda
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir request-fn)
        (set! probed (cons (chain-node-provider node) probed))
        (equal? (chain-node-provider node) "google1")
      ) ;lambda
      #f
      135000
    ) ;pi-chain-execute
    =>
    'executed-on-node-3-again
  ) ;check
  ;; 粘性节点 kimi1 故障；deepseek1 超出冷却期探活失败；顺延探测 google1（成功）
  (check (reverse probed) => '("kimi1" "deepseek1" "google1"))
  (check executed => "google1")
  (check (pi-chain-node-last-failure N1) => 135000)
  (check (pi-chain-node-last-failure N2) => 135000)
) ;let

;; 6. 全节点在冷却期内时不发起任何探活，立即返回枯竭，避免连续雪崩超时
(let ((probed '()) (called? #f))
  (pi-chain-record-node-failure! N3 135100)
  (pi-chain-clear-sticky-node! "coding-deep")
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda args (set! called? #t) 'unexpected)
      CFG
      TMP-DIR
      #f
      (lambda (node timeout-ms config-dir request-fn)
        (set! probed (cons (chain-node-provider node) probed))
        #f
      ) ;lambda
      #f
      136000
    ) ;pi-chain-execute
    =>
    'all-nodes-exhausted
  ) ;check
  ;; 三个节点均在 60 秒冷却期内（故障时间戳 135000/135000/135100，当前 136000），探活列表为空
  (check probed => '())
  (check-false called?)
) ;let

;; 7. 冷却期禁用（retryIntervalMs <= 0）：节点不进入冷却期，保持立即探活
(let ((probed '())
      (cfg-no-cd (make-all-in-one-cfg #f 1000 0 3456 (list CHAIN)))
     ) ;
  (pi-chain-reset-rewind-state!)
  (pi-chain-record-node-failure! N1 100)
  (check
    (pi-chain-execute CHAIN
      (list USER)
      (lambda (provider . args) provider)
      cfg-no-cd
      TMP-DIR
      #f
      (lambda (node . args)
        (set! probed (cons (chain-node-provider node) probed))
        #t
      ) ;lambda
      #f
      105
    ) ;pi-chain-execute
    =>
    "kimi1"
  ) ;check
  (check (reverse probed) => '("kimi1"))
) ;let

;; 8. 空链直接枯竭；参数类型错误明确暴露
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
