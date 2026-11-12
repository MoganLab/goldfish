;; recovery 等待事件与 print / JSON 模式的 stdout 隔离测试
;; 倒计时与恢复状态只能走日志或 stderr，不得污染脚本模式的 stdout。
;;
;; 语法
;; ----
;; (emit-json-event type . args)
;; (run-agent-loop-with-timeout cfg init-msgs system-prompt emit parsed
;;   compaction emit-initial? probe-fn request-fn)
;;
;; 参数
;; ----
;; cfg : alist，含注入的 now-fn / sleep-fn / aborted? 与 all-in-one 供应商。
;; init-msgs : list，初始消息列表。
;; system-prompt : string，系统提示词。
;; emit : procedure，print 模式为事件收集器，JSON 模式为 emit-json-event。
;; parsed : list，解析后的命令行参数（此处为空）。
;; probe-fn、request-fn : procedure，stub 探活与业务请求。
;;
;; 返回值
;; ----
;; string
;; with-output-to-string 捕获到的 stdout 内容，用于断言不含倒计时噪声。

(import (scheme base)
  (scheme file)
  (scheme write)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi ai event)
  (liii pi ai message)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-aio-stdout-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path (path-join TMP-DIR "models" "stub")) :parents #t :exist-ok #t)
(path-write-text (path->string (path-join TMP-DIR "all-in-one.json"))
  "{\"provider\":\"all-in-one\",\"continuous\":true,\"retryIntervalMs\":400,\"maxWaitMs\":4000,\"probeTimeoutMs\":1000,\"chains\":[{\"id\":\"coding-fast\",\"nodes\":[{\"provider\":\"stub\",\"model\":\"one\"}]}]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "providers.json"))
  "[\"stub\"]"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "auth.json"))
  "{\"stub\":{\"type\":\"api_key\",\"key\":\"k\"}}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub.json"))
  "{\"api\":\"openai-completions\",\"baseUrl\":\"http://127.0.0.1:1\",\"models\":[\"one\"]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "one.json"))
  "{\"id\":\"one\",\"name\":\"one\",\"reasoning\":true}"
) ;path-write-text

;; 虚拟时钟：等待立即推进，不真实阻塞

(define now 1000000)

;; print / JSON 模式的 emit 与 run-agent-loop-with-timeout 一致：只经 emit-json-event 输出
;; recovery 事件不属于协议事件，stdout 必须保持干净

(define (capture-stdout thunk)
  (with-output-to-string thunk)
) ;define

;; 1. recovery 事件经 JSON 事件通道不产生任何 stdout 输出
(let ((out
        (capture-stdout
          (lambda ()
            (for-each (lambda (ev) (apply emit-json-event ev))
              (list (list 'recovery_wait "coding-fast" 3000 3000)
                (list 'recovery_tick "coding-fast" 2000)
                (list 'recovery_retry "coding-fast")
                (list 'recovery_end "coding-fast" 'success)
              ) ;list
            ) ;for-each
          ) ;lambda
        ) ;capture-stdout
      ) ;out
     ) ;
  (check out => "")
) ;let

;; 2. print 模式经全链枯竭 → recovery 等待 → 恢复后结算：
;;    stdout 只有最终 assistant 文本，不含倒计时或事件噪声
(pi-chain-reset-sticky-state!)
(let* ((probe-round 0)
       (cfg
         (list (cons 'tools '())
           (cons 'provider "all-in-one")
           (cons 'provider-api "all-in-one")
           (cons 'model-id "coding-fast")
           (cons 'api-key #f)
           (cons 'config-dir TMP-DIR)
           (cons 'now-fn (lambda () now))
           (cons 'sleep-fn (lambda (ms) (set! now (+ now ms))))
           (cons 'aborted? (lambda () #f))
         ) ;list
       ) ;cfg
       (evs '())
       (emit (lambda (ev-type . args) (set! evs (cons ev-type evs))))
       (parsed '())
       (out
         (capture-stdout
           (lambda ()
             (run-agent-loop-with-timeout cfg
               (list (pi-user-message "print mode work"))
               ""
               emit
               parsed
               #f
               #f
               (lambda (node timeout-ms dir request-fn)
                 (set! probe-round (+ probe-round 1))
                 (>= probe-round 2)
               ) ;lambda
               (lambda (provider node-cfg messages system-prompt tools on-event timeout)
                 (pi-assistant-message (list (pi-text-block "print-recovered")) "stop")
               ) ;lambda
             ) ;run-agent-loop-with-timeout
           ) ;lambda
         ) ;capture-stdout
       ) ;out
      ) ;
  ;; stdout 中不含倒计时或 recovery 事件名
  (check-false (string-contains? out "Retrying in"))
  (check-false (string-contains? out "recovery_wait"))
  ;; 事件序列中仍能观察到 recovery 过程（供 TUI 使用，不写 stdout）
  (check-true (if (memq 'recovery_wait evs) #t #f))
  (check-true (if (memq 'recovery_end evs) #t #f))
) ;let*

;; 3. JSON 模式：recovery 期间 stdout 仍是逐行合法 JSON（协议事件），不含倒计时
(pi-chain-reset-sticky-state!)
(let* ((probe-round 0)
       (cfg
         (list (cons 'tools '())
           (cons 'provider "all-in-one")
           (cons 'provider-api "all-in-one")
           (cons 'model-id "coding-fast")
           (cons 'api-key #f)
           (cons 'config-dir TMP-DIR)
           (cons 'now-fn (lambda () now))
           (cons 'sleep-fn (lambda (ms) (set! now (+ now ms))))
           (cons 'aborted? (lambda () #f))
         ) ;list
       ) ;cfg
       (out
         (capture-stdout
           (lambda ()
             (run-agent-loop-with-timeout cfg
               (list (pi-user-message "json mode work"))
               ""
               emit-json-event
               '()
               #f
               #f
               (lambda (node timeout-ms dir request-fn)
                 (set! probe-round (+ probe-round 1))
                 (>= probe-round 2)
               ) ;lambda
               (lambda (provider node-cfg messages system-prompt tools on-event timeout)
                 (pi-assistant-message (list (pi-text-block "json-recovered")) "stop")
               ) ;lambda
             ) ;run-agent-loop-with-timeout
           ) ;lambda
         ) ;capture-stdout
       ) ;out
      ) ;
  (check-false (string-contains? out "Retrying in"))
  (check-false (string-contains? out "recovery_tick"))
  ;; 每行都是合法 JSON 对象（首字符 {）
  (let loop
    ((lines (string-split out "\n")) (n 0))
    (cond ((null? lines) (check-true (> n 0)))
          ((string-null? (car lines)) (loop (cdr lines) n))
          (else (check (string-ref (car lines) 0) => #\{) (loop (cdr lines) (+ n 1)))
    ) ;cond
  ) ;let
) ;let*

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
