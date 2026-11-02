;; pi-fail-tool-calls-length
;; 为因模型输出被 Token 限制截断而无法执行的工具调用生成跳过失败结果消息。
;;
;; 语法
;; ----
;; (pi-fail-tool-calls-length tool-calls emit)
;;
;; 参数
;; ----
;; tool-calls : list?，被截断未执行的工具调用块列表。
;; emit : (or procedure? #f)，事件派发过程。
;;
;; 返回值
;; ----
;; list?，生成的 toolResult 失败消息列表。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 为被截断的工具调用生成失败结果，并触发事件
(let* ((evs '())
       (emit
         (lambda (ev-type msg) (set! evs (cons (cons ev-type msg) evs)))
       ) ;emit
       (calls (list '((id . "call_1") (name . "bash")) '((id . "call_2")
                                                         (name . "edit")))
       ) ;calls
       (results (pi-fail-tool-calls-length calls emit))
      ) ;
  (check (length results) => 2)
  (check-true (pi-tool-result-message? (car results)))
  (check (pi-tool-result-message-tool-call-id (car results)) => "call_1")
  (check (pi-tool-result-message-is-error (car results)) => #t)
  (let ((content (pi-tool-result-message-content (car results))))
    (check (pi-text-block-text (car content))
      =>
      "Tool call skipped: model output truncated by token limit"
    ) ;check
  ) ;let
  ;; 验证每个消息派发了 message_start 和 message_end
  (check (length evs) => 4)
) ;let*

;; 2. emit 为 #f 正常运行不报错
(let* ((calls (list '((id . "call_3") (name . "read"))))
       (results (pi-fail-tool-calls-length calls #f))
      ) ;
  (check (length results) => 1)
  (check (pi-tool-result-message-tool-name (car results)) => "read")
) ;let*

;; 3. 空输入返回空列表
(check (pi-fail-tool-calls-length '() #f) => '())

(check-report)
