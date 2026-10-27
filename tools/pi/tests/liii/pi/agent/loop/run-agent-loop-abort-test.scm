(import (scheme base)
  (liii go)
  (liii list)
  (liii check)
  (liii pi abort)
  (liii pi agent loop)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 构造 stop-reason 为 toolUse 的 assistant 响应（alist 形式，对齐
;; pi-step-agent-turn 的 pi-agent-assoc-val 读取约定）
(define (make-tool-use-response tool-name)
  (pi-assistant-message
    (list (list (cons 'type "text") (cons 'text "calling tool"))
      (list (cons 'type "toolCall")
        (cons 'id "call_1")
        (cons 'name tool-name)
        (cons 'arguments (list (cons "command" "echo hi")))
      ) ;list
    ) ;list
    :stop-reason "toolUse"
    :usage '((input . 1) (output . 1) (total . 2))
  ) ;pi-assistant-message
) ;define

;; 收集 emit 事件序列：(emit 'type . args)，特殊符号 'dump 取出逆序事件表
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
  (let loop ((rest events))
    (cond ((null? rest) #f)
          ((eq? (caar rest) 'agent_settled) (cdar rest))
          (else (loop (cdr rest)))
    ) ;cond
  ) ;loop
) ;define

;; 1. turn 开始前已中止：request-fn 不被调用，agent_settled 带 aborted=#t
(let* ((ch (make-chan 1))
       (req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (pi-assistant-message '()
             :stop-reason "stop"
             :usage '((input . 1) (output . 1) (total . 2))
           ) ;pi-assistant-message
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test") (cons 'tools '()) (cons 'settings '())))
      ) ;
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
    600 #f request-fn
  ) ;run-agent-loop
  (check (vector-ref req-count 0) => 0)
  (let ((settled (find-settled (emit 'dump))))
    (check settled => '(#t))
  ) ;let
) ;let*

;; 2. 工具执行中触发中止：下一轮不再发起请求，agent_settled 带 aborted=#t
(let* ((ch (make-chan 1))
       (req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (make-tool-use-response "bash")
         ) ;lambda
        )
       (exec
         (lambda (name . args)
           ;; 第一个工具执行时触发中止信号（模拟用户在工具执行期间按 ESC）
           (chan-send! ch 'abort 0)
           (list (cons 'content
                  (list (list (cons 'type "text") (cons 'text "tool output")))
                ) ;list
             (cons 'isError #f)
           ) ;list
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test")
              (cons 'tools '("bash"))
              (cons 'settings '())
              (cons 'execute-tool-call exec)
            ) ;list
           )
        ) ;
  (pi-abort-arm-chan! ch)
  (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
    600 #f request-fn
  ) ;run-agent-loop
  ;; 第 1 轮请求发生，第 2 轮前检测到中止不再请求
  (check (vector-ref req-count 0) => 1)
  (let ((settled (find-settled (emit 'dump))))
    (check settled => '(#t))
  ) ;let
  ;; 工具确实被执行了 1 次（tool_execution_end 事件存在）
  (check-true
    (pair?
      (filter
        (lambda (ev) (eq? (car ev) 'tool_execution_end))
        (emit 'dump)
      ) ;filter
    ) ;pair?
  ) ;check-true
) ;let*

;; 3. 未中止时行为不变：request-fn 正常调用且 agent_settled 带 aborted=#f
(let* ((ch (make-chan 1))
       (req-count (vector 0))
       (request-fn
         (lambda (provider config msgs sys-prompt tools on-ev timeout)
           (vector-set! req-count 0 (+ 1 (vector-ref req-count 0)))
           (pi-assistant-message '()
             :stop-reason "stop"
             :usage '((input . 1) (output . 1) (total . 2))
           ) ;pi-assistant-message
         ) ;lambda
        )
       (emit (make-emit-collector))
       (cfg (list (cons 'provider "test") (cons 'tools '()) (cons 'settings '())))
      ) ;
  (pi-abort-arm-chan! ch)
  (run-agent-loop cfg (list (pi-user-message "hi")) "sys" emit 10 5
    600 #f request-fn
  ) ;run-agent-loop
  (check (vector-ref req-count 0) => 1)
  (let ((settled (find-settled (emit 'dump))))
    (check settled => '(#f))
  ) ;let
) ;let*

(check-report)
