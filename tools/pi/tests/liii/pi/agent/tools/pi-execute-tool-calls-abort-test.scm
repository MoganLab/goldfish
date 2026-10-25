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
                   (cons 'text
                     (string-append "out-"
                       (number->string (vector-ref exec-count 0))
                     ) ;string-append
                   ) ;cons
                 ) ;list
               ) ;list
             ) ;cons
             (cons 'isError #f)
           ) ;list
         ) ;lambda
        )
       (cfg (list (cons 'execute-tool-call exec)))
       (calls (list (make-call "call_1" "bash" "echo 1")
                (make-call "call_2" "bash" "echo 2")
                (make-call "call_3" "bash" "echo 3")
              ) ;list
           )
       (events '())
       (emit (lambda (ev-type . args)
               (set! events (cons (cons ev-type args) events))
               #t
             ) ;lambda
           )
      ) ;
  (pi-abort-arm-chan! ch)
  (let-values (((res term?)
                (pi-execute-tool-calls-with-status calls emit 10 cfg)
               ) ;
              ) ;
    ;; 只有第 1 个工具真正执行
    (check (vector-ref exec-count 0) => 1)
    ;; 3 个工具结果都在（保证消息序列完整）
    (check (length res) => 3)
    ;; 第 1 个为正常结果
    (let ((c1 (pi-tool-result-message-content (car res))))
      (check (pi-text-block-text (car c1)) => "out-1")
    ) ;let
    ;; 第 2、3 个为 aborted 跳过结果（isError，文本含 aborted 标识）
    (for-each
      (lambda (msg)
        (let ((c (pi-tool-result-message-content msg)))
          (check-true (string-contains? (pi-text-block-text (car c)) "aborted"))
          (check-true (pi-tool-result-message-is-error msg))
        ) ;let
      ) ;lambda
      (list (cadr res) (caddr res))
    ) ;for-each
    ;; 3 个工具都发了 tool_execution_end 事件（保持事件序列对称）
    (check
      (length
        (filter (lambda (ev) (eq? (car ev) 'tool_execution_end)) (reverse events))
      ) ;length
      => 3
    ) ;check
    ;; 中止不是 terminate：term? 保持 #f
    (check term? => #f)
  ) ;let-values
) ;let*

;; 2. 未中止时批量执行行为不变：全部实际执行
(let* ((ch (make-chan 1))
       (exec-count (vector 0))
       (exec
         (lambda (name . args)
           (vector-set! exec-count 0 (+ 1 (vector-ref exec-count 0)))
           (list
             (cons 'content
               (list
                 (list (cons 'type "text")
                   (cons 'text
                     (string-append "out-"
                       (number->string (vector-ref exec-count 0))
                     ) ;string-append
                   ) ;cons
                 ) ;list
               ) ;list
             ) ;cons
             (cons 'isError #f)
           ) ;list
         ) ;lambda
        )
       (cfg (list (cons 'execute-tool-call exec)))
       (calls (list (make-call "call_1" "bash" "echo 1")
                (make-call "call_2" "bash" "echo 2")
              ) ;list
           )
      ) ;
  (pi-abort-arm-chan! ch)
  (let-values (((res term?)
                (pi-execute-tool-calls-with-status calls #f 10 cfg)
               ) ;
              ) ;
    (check (vector-ref exec-count 0) => 2)
    (check (length res) => 2)
    (let ((c2 (pi-tool-result-message-content (cadr res))))
      (check (pi-text-block-text (car c2)) => "out-2")
    ) ;let
  ) ;let-values
) ;let*

(check-report)
