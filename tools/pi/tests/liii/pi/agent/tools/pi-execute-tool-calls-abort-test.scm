;; pi-execute-tool-calls-abort
;; 测试批量工具调用在执行过程中收到用户中止（ESC / abort）信号时的跳过处理与事件派发。
;;
;; 语法
;; ----
;; (pi-execute-tool-calls-with-status calls emit timeout cfg)
;;
;; 参数
;; ----
;; calls : list?，待执行的工具调用块列表。
;; emit : (or procedure? #f)，事件派发过程。
;; timeout : real?，工具执行超时时间（秒）。
;; cfg : list?，包含 execute-tool-call 的 Agent 配置 alist。
;;
;; 返回值
;; ----
;; (values list? boolean?)，返回工具结果消息列表与批次终止标记。
;;
;; 说明
;; ----
;; 当第 1 个工具执行期间触发中止信号后，后续工具应被自动跳过并标记为 Tool call aborted by user。

(import (scheme base)
  (liii go)
  (liii list)
  (liii check)
  (liii string)
  (liii pi abort)
  (liii pi agent tools)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; 构造 toolCall block

(define (make-call id name command)
  (list (cons 'type "toolCall")
    (cons 'id id)
    (cons 'name name)
    (cons 'arguments (list (cons "command" command)))
  ) ;list
) ;define

;; 1. 批量工具执行中途中止：第 1 个工具执行时触发中止信号，
;;    其余工具不再实际执行，标记为 aborted 跳过结果
(let* ((ch (make-chan 1))
       (exec-count (vector 0))
       (exec
         (lambda (name . args)
           (vector-set! exec-count 0 (+ 1 (vector-ref exec-count 0)))
           ;; 第 1 个工具执行时触发中止（模拟用户在工具执行期间按 ESC）
           (when (= 1 (vector-ref exec-count 0))
             (chan-send! ch 'abort 0)
           ) ;when
           (list
             (cons 'content
               (list
                 (list (cons 'type "text")
                   (cons 'text (string-append "out-" (number->string (vector-ref exec-count 0))))
                 ) ;list
               ) ;list
             ) ;cons
             (cons 'isError #f)
           ) ;list
         ) ;lambda
       ) ;exec
       (cfg (list (cons 'execute-tool-call exec)))
       (calls (list (make-call "call_1" "bash" "echo 1")
                (make-call "call_2" "bash" "echo 2")
                (make-call "call_3" "bash" "echo 3")
              ) ;list
       ) ;calls
       (events '())
       (emit
         (lambda (ev-type . args) (set! events (cons (cons ev-type args) events)) #t)
       ) ;emit
      ) ;
  (pi-abort-arm-chan! ch)
  (let-values (((res term?) (pi-execute-tool-calls-with-status calls emit 10 cfg)))
    ;; 只有第 1 个工具真正执行
    (check (vector-ref exec-count 0) => 1)
    ;; 3 个工具结果都在（保证消息序列完整）
    (check (length res) => 3)
    ;; 第 1 个是正常输出
    (let ((c1 (pi-tool-result-message-content (car res))))
      (check (pi-text-block-text (car c1)) => "out-1")
      (check (pi-tool-result-message-is-error (car res)) => #f)
    ) ;let
    ;; 第 2、3 个标记为被用户中止
    (let ((c2 (pi-tool-result-message-content (cadr res)))
          (c3 (pi-tool-result-message-content (caddr res)))
         ) ;
      (check (pi-text-block-text (car c2)) => "Tool call aborted by user")
      (check (pi-tool-result-message-is-error (cadr res)) => #t)
      (check (pi-text-block-text (car c3)) => "Tool call aborted by user")
      (check (pi-tool-result-message-is-error (caddr res)) => #t)
    ) ;let
    ;; 批次未被扩展 terminate，batch-term? 仍为 #f
    (check term? => #f)
    ;; 事件流完整性：每个工具都有完整的生命周期事件
    (let ((starts
            (filter (lambda (e) (eq? (car e) 'tool_execution_start)) events)
          ) ;starts
          (ends
            (filter (lambda (e) (eq? (car e) 'tool_execution_end)) events)
          ) ;ends
          (msg-starts
            (filter (lambda (e) (eq? (car e) 'message_start)) events)
          ) ;msg-starts
          (msg-ends
            (filter (lambda (e) (eq? (car e) 'message_end)) events)
          ) ;msg-ends
         ) ;
      (check (length starts) => 3)
      (check (length ends) => 3)
      (check (length msg-starts) => 3)
      (check (length msg-ends) => 3)
    ) ;let
  ) ;let-values
  (pi-abort-reset!)
) ;let*

;; 2. 预先中止状态下批量执行：所有工具均直接被跳过，不再执行任何实际逻辑
(let* ((exec-called #f)
       (exec (lambda (name . args) (set! exec-called #t) '()))
       (cfg (list (cons 'execute-tool-call exec)))
       (calls (list (make-call "call_a" "bash" "echo a") (make-call "call_b" "bash" "echo b"))
       ) ;calls
      ) ;
  ;; 直接触发中止
  (let ((ch (make-chan 1)))
    (pi-abort-arm-chan! ch)
    (chan-send! ch 'abort 0)
    (pi-aborted?)
  ) ;let
  (let-values (((res term?) (pi-execute-tool-calls-with-status calls #f 10 cfg)))
    (check exec-called => #f)
    (check (length res) => 2)
    (check (pi-tool-result-message-is-error (car res)) => #t)
    (check (pi-tool-result-message-is-error (cadr res)) => #t)
    (check
      (pi-text-block-text (car (pi-tool-result-message-content (car res))))
      =>
      "Tool call aborted by user"
    ) ;check
  ) ;let-values
  (pi-abort-reset!)
) ;let*

(check-report)
