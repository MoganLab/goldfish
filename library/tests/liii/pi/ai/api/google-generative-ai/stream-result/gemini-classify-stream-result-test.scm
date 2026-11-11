;; gemini-classify-stream-result
;; 根据流式请求的结束状态判定成功或失败，并生成对应的 assistant 消息。
;;
;; 语法
;; ----
;; (gemini-classify-stream-result stream-state idle-timed-out? resp status eff-idle on-event error-body-acc)
;;
;; 参数
;; ----
;; stream-state : gemini-stream-state | vector
;; 可变流状态；record 为当前表示，vector 用于兼容旧调用。
;; idle-timed-out? : boolean
;; 是否因流空闲超时而中断。
;; resp : alist | procedure | hash-table?
;; 响应对象，可提供 error、reason、body 或 text 字段。
;; status : integer
;; HTTP 状态码；0 表示网络错误或连接失败。
;; eff-idle : number
;; 生效的空闲超时秒数，用于生成超时提示。
;; on-event : procedure | #f
;; 接收流式事件的回调；成功结算时用于结束当前活动块。
;; error-body-acc : vector
;; 长度为 1 的向量，保存流式过程中累积的响应体原文。
;;
;; 返回值
;; ----
;; pi-assistant-message
;; 包含 content、stopReason、usage 与可选 errorMessage 的消息 record。
;;
;; 说明
;; ----
;; 空闲超时、状态码非 200 与状态码为 0 分别对应三类失败；
;; 其余情况按成功结算。

(import (liii check)
  (liii string)
  (liii pi ai event)
  (liii pi ai message)
  (liii pi ai api google-generative-ai stream-result)
  (liii pi ai api google-generative-ai stream-state)
) ;import

(check-set-mode! 'report-failed)

;; 成功结算
(let ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (let ((msg (gemini-classify-stream-result stream-state #f '() 200 60 #f (vector "")))
       ) ;
    (check (cdr (assoc 'stopReason msg)) => "stop")
  ) ;let
) ;let

;; 空闲超时
(let ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (let ((msg (gemini-classify-stream-result stream-state #t '() 200 60 #f (vector "")))
       ) ;
    (check (cdr (assoc 'stopReason msg)) => "error")
    (check-true (string-contains? (cdr (assoc 'errorMessage msg))
                  "Stream idle timed out after 60 seconds"
                ) ;string-contains?
    ) ;check-true
  ) ;let
) ;let

;; 错误描述命中超时关键词
(let ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (let ((msg (gemini-classify-stream-result stream-state
               #f
               '((error . "request too slow"))
               200
               30
               #f
               (vector "")
             ) ;gemini-classify-stream-result
        ) ;msg
       ) ;
    (check (cdr (assoc 'stopReason msg)) => "error")
    (check-true (string-contains? (cdr (assoc 'errorMessage msg)) "Stream idle timed out")
    ) ;check-true
  ) ;let
) ;let

;; HTTP 500 且响应体带错误信息
(let ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (let ((msg (gemini-classify-stream-result stream-state
               #f
               '()
               500
               60
               #f
               (vector "{\"error\":{\"message\":\"boom\"}}")
             ) ;gemini-classify-stream-result
        ) ;msg
       ) ;
    (check (cdr (assoc 'stopReason msg)) => "error")
    (check (cdr (assoc 'errorMessage msg)) => "HTTP 500: boom")
  ) ;let
) ;let

;; HTTP 500 且响应体无错误信息
(let ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (let ((msg (gemini-classify-stream-result stream-state #f '() 500 60 #f (vector "")))
       ) ;
    (check (cdr (assoc 'stopReason msg)) => "error")
    (check (cdr (assoc 'errorMessage msg)) => "HTTP 500 error")
  ) ;let
) ;let

;; 状态码为 0 表示网络错误
(let ((stream-state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (let ((msg (gemini-classify-stream-result stream-state
               #f
               '((reason . "connection refused"))
               0
               60
               #f
               (vector "")
             ) ;gemini-classify-stream-result
        ) ;msg
       ) ;
    (check (cdr (assoc 'stopReason msg)) => "error")
    (check (cdr (assoc 'errorMessage msg)) => "Request failed: connection refused")
  ) ;let
) ;let

(check-report)
