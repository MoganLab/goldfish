(import (scheme base)
  (liii go)
  (liii list)
  (liii check)
  (liii pi abort)
  (liii pi agent loop)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

(define (make-text-response text)
  (pi-assistant-message (list (list (cons 'type "text") (cons 'text text)))
    :stop-reason "stop"
    :usage       '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (make-aborted-response)
  (pi-assistant-message '()
    :stop-reason "aborted"
    :usage       '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (make-tool-use-response)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text "calling tool"))
      (list (cons 'type "toolCall")
        (cons 'id "call_1")
        (cons 'name "bash")
        (cons 'arguments (list (cons "command" "echo hi")))
      ) ;list
    ) ;list
    :stop-reason "toolUse"
    :usage       '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

(define (make-emit-collector)
  (let ((events '()))
    (lambda (ev-type . args)
      (if (eq? ev-type 'dump)
        (reverse events)
        (begin
          (set! events (cons (cons ev-type args) events))
          #t
        ) ;begin
      ) ;if
    ) ;lambda
  ) ;let
) ;define

(define (find-settled events)
  (let loop
    ((rest events))
    (cond ((null? rest) #f)
          ((eq? (caar rest) 'agent_settled) (cdar rest))
          (else (loop (cdr rest)))
    ) ;cond
  ) ;let
) ;define

(define (ok-exec name . args)
  (list
    (cons 'content (list (list (cons 'type "text") (cons 'text "tool output"))))
    (cons 'isError #f)
  ) ;list
) ;define

;; 每个用例独立 arm 全局 abort 状态：防止前序用例的一次性中止标志
;; 污染后续用例（对齐 run-agent-loop-abort-test.scm 的做法）

(define (arm-fresh!)
  (pi-abort-arm-chan! (make-chan 1))
) ;define

;; 构造带两个轮询回调计数的 cfg

(define (make-cfg . extra)
  (append (list (cons 'provider "test") (cons 'tools '()) (cons 'settings '()))
    extra
  ) ;append
) ;define

;; -----------------------------------------------------------------------------
;; 测试项 1: ESC 中止（首轮请求前）：steering / follow-up 均不被轮询
;; -----------------------------------------------------------------------------
(let* ((ch (make-chan 1))
       (req-count (vector 0))
       (steer-count (vector 0))
       (fu-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "done")
         ) ;lambda
       ) ;request-fn
       (emit (make-emit-collector))
       (cfg
         (make-cfg
           (cons 'get-steering-messages
             (lambda ()
               (vector-set! steer-count 0 (+ 1 (vector-ref steer-count 0)))
               '("中止前不该轮询")
             ) ;lambda
           ) ;cons
           (cons 'get-follow-up-messages
             (lambda ()
               (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
               '("中止前不该轮询")
             ) ;lambda
           ) ;cons
         ) ;make-cfg
       ) ;cfg
      ) ;
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (let ((res-msgs (run-agent-loop cfg
                    (list (pi-user-message "hi"))
                    "sys"
                    emit
                    10
                    5
                    600
                    #f
                    request-fn
                  ) ;run-agent-loop
        ) ;res-msgs
       ) ;
    (check (vector-ref req-count 0) => 0)
    (check (vector-ref steer-count 0) => 0)
    (check (vector-ref fu-count 0) => 0)
    (let ((settled (find-settled (emit 'dump))))
      (check settled => '(#t))
    ) ;let
    (check (length res-msgs) => 1)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 2: 请求返回 aborted 结算：硬退出，不轮询 steering / follow-up
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (steer-count (vector 0))
       (fu-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-aborted-response)
         ) ;lambda
       ) ;request-fn
       (emit (make-emit-collector))
       (cfg
         (make-cfg
           (cons 'get-steering-messages
             (lambda ()
               (vector-set! steer-count 0 (+ 1 (vector-ref steer-count 0)))
               '("aborted 后不该轮询")
             ) ;lambda
           ) ;cons
           (cons 'get-follow-up-messages
             (lambda ()
               (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
               '("aborted 后不该轮询")
             ) ;lambda
           ) ;cons
         ) ;make-cfg
       ) ;cfg
      ) ;
  (pi-abort-arm-chan! (make-chan 1))
  (let ((res-msgs (run-agent-loop cfg
                    (list (pi-user-message "hi"))
                    "sys"
                    emit
                    10
                    5
                    600
                    #f
                    request-fn
                  ) ;run-agent-loop
        ) ;res-msgs
       ) ;
    (check (vector-ref req-count 0) => 1)
    ;; 轮询点 1 在请求前发生一次（允许），但停止路径不再轮询：
    ;; steering 恰好 1 次（请求前），follow-up 0 次
    (check (vector-ref steer-count 0) => 1)
    (check (vector-ref fu-count 0) => 0)
    ;; hi + 请求前注入的 steering + aborted assistant 共 3 条
    (check (length res-msgs) => 3)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 3: max-turns 命中：不再轮询，直接以错误结算
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (steer-count (vector 0))
       (fu-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-tool-use-response)
         ) ;lambda
       ) ;request-fn
       (emit (make-emit-collector))
       (cfg
         (append
           (make-cfg (cons 'tools '("bash"))
             (cons 'execute-tool-call ok-exec)
             (cons 'get-steering-messages
               (lambda () (vector-set! steer-count 0 (+ 1 (vector-ref steer-count 0))) '())
             ) ;cons
             (cons 'get-follow-up-messages
               (lambda ()
                 (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
                 '("max-turns 后不该轮询")
               ) ;lambda
             ) ;cons
           ) ;make-cfg
         ) ;append
       ) ;cfg
      ) ;
  ;; max-turns=1：第 1 轮工具后递归即命中上限
  (arm-fresh!)
  (let ((res-msgs (run-agent-loop cfg
                    (list (pi-user-message "hi"))
                    "sys"
                    emit
                    10
                    1
                    600
                    #f
                    request-fn
                  ) ;run-agent-loop
        ) ;res-msgs
       ) ;
    (check (vector-ref req-count 0) => 1)
    ;; steering 仅在第 1 轮请求前轮询 1 次；follow-up 从未轮询
    (check (vector-ref steer-count 0) => 1)
    (check (vector-ref fu-count 0) => 0)
    ;; max-turns 错误结算：hi + assistant + toolResult + 错误 assistant 共 4 条
    (check (length res-msgs) => 4)
  ) ;let
) ;let*

;; -----------------------------------------------------------------------------
;; 测试项 4: 会话超时命中：不轮询，直接以错误结算
;; -----------------------------------------------------------------------------
(let* ((req-count (vector 0))
       (fu-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-text-response "done")
         ) ;lambda
       ) ;request-fn
       (emit (make-emit-collector))
       (cfg
         (make-cfg
           (cons 'get-follow-up-messages
             (lambda ()
               (vector-set! fu-count 0 (+ 1 (vector-ref fu-count 0)))
               '("超时后不该轮询")
             ) ;lambda
           ) ;cons
         ) ;make-cfg
       ) ;cfg
      ) ;
  ;; session-timeout=-1：首轮即命中会话超时
  (arm-fresh!)
  (let ((res-msgs (run-agent-loop cfg
                    (list (pi-user-message "hi"))
                    "sys"
                    emit
                    10
                    5
                    -1
                    #f
                    request-fn
                  ) ;run-agent-loop
        ) ;res-msgs
       ) ;
    (check (vector-ref req-count 0) => 0)
    (check (vector-ref fu-count 0) => 0)
    ;; 超时错误结算：hi + 错误 assistant 共 2 条
    (check (length res-msgs) => 2)
  ) ;let
) ;let*

(check-report)
