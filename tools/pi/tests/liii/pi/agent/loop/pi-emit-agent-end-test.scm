;; pi-emit-agent-end
;; 派发 Agent 会话结束事件 agent_end 与结算事件 agent_settled，携带总费用与中止状态。
;;
;; 语法
;; ----
;; (pi-emit-agent-end emit final-msgs stop-reason config)
;;
;; 参数
;; ----
;; emit : (or procedure? #f)，事件派发过程。
;; final-msgs : list?，当前会话的完整消息历史列表。
;; stop-reason : string?，会话停止原因（如 "stop"、"aborted" 等）。
;; config : list?，包含模型与费率信息的 Agent 配置 alist。
;;
;; 返回值
;; ----
;; unspecified，若 emit 为过程则派发 agent_end 与 agent_settled 事件，否则无操作。

(import (scheme base) (liii check) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 正常派发 agent_end 与 agent_settled 事件
(let* ((evs '())
       (mock-emit
         (lambda (ev-type . args) (set! evs (cons (cons ev-type args) evs)))
       ) ;mock-emit
       (msgs '())
       (cfg '((provider . "google")
              (model-id . "gemini-3.8-flash")
              (models-store)))
      ) ;
  (pi-emit-agent-end mock-emit msgs "stop" cfg)
  (check (length evs) => 2)
  (let ((rev-evs (reverse evs)))
    (check (caar rev-evs) => 'agent_end)
    (check (caadr rev-evs) => 'agent_settled)
    (check (cadadr rev-evs) => #f)
  ) ;let
) ;let*

;; 2. 被中止停止（stop-reason 为 "aborted"）时 agent_settled 携带 #t
(let* ((evs '())
       (mock-emit
         (lambda (ev-type . args) (set! evs (cons (cons ev-type args) evs)))
       ) ;mock-emit
       (cfg '((provider . "google")
              (model-id . "gemini-3.8-flash")
              (models-store)))
      ) ;
  (pi-emit-agent-end mock-emit '() "aborted" cfg)
  (let ((rev-evs (reverse evs)))
    (check (cadadr rev-evs) => #t)
  ) ;let
) ;let*

;; 3. emit 为 #f 时无操作不报错
(check-true (begin (pi-emit-agent-end #f '() "stop" '()) #t))

(check-report)
