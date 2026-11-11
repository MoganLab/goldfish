;; make-gemini-stream-state
;; 创建 Gemini 流状态记录，保存流式过程中的消息块、文本、用量与当前块状态。
;;
;; 语法
;; ----
;; (make-gemini-stream-state blocks last-text usage tool-calls finish-reason active-type thinking-text thinking-signature)
;;
;; 参数
;; ----
;; blocks : list
;; 已完成的 pi 消息块列表。
;; last-text : string | #f
;; 当前正在累积的文本；没有活动文本块时为 #f。
;; usage : alist
;; 已统计的用量 alist。
;; tool-calls : list
;; 已完成的 pi-tool-call-block 列表。
;; finish-reason : string | #f
;; Gemini 返回的原始 finishReason。
;; active-type : string | #f
;; 当前活动块类型，可取 text 或 thinking。
;; thinking-text : string
;; 当前正在累积的思考文本。
;; thinking-signature : string
;; 当前思考块的签名。
;;
;; 返回值
;; ----
;; gemini-stream-state
;; 流状态记录。

(import (liii check) (liii pi ai api google-generative-ai stream-state))

(check-set-mode! 'report-failed)

;; 新建的流状态各字段与入参一致
(let ((state (make-gemini-stream-state '() #f '() '() "STOP" #f "" "")))
  (check (gemini-stream-state? state) => #t)
  (check (gemini-stream-state-blocks state) => '())
  (check (gemini-stream-state-last-text state) => #f)
  (check (gemini-stream-state-usage state) => '())
  (check (gemini-stream-state-tool-calls state) => '())
  (check (gemini-stream-state-finish-reason state) => "STOP")
  (check (gemini-stream-state-current-active-type state) => #f)
  (check (gemini-stream-state-thinking-text state) => "")
  (check (gemini-stream-state-thinking-signature state) => "")
) ;let

;; 入参非默认值时同样被保留
(let ((state (make-gemini-stream-state '(blk)
               "text"
               '(usage)
               '(tc)
               "MAX_TOKENS"
               "thinking"
               "why"
               "sig"
             ) ;make-gemini-stream-state
      ) ;state
     ) ;
  (check (gemini-stream-state-blocks state) => '(blk))
  (check (gemini-stream-state-last-text state) => "text")
  (check (gemini-stream-state-usage state) => '(usage))
  (check (gemini-stream-state-tool-calls state) => '(tc))
  (check (gemini-stream-state-finish-reason state) => "MAX_TOKENS")
  (check (gemini-stream-state-current-active-type state) => "thinking")
  (check (gemini-stream-state-thinking-text state) => "why")
  (check (gemini-stream-state-thinking-signature state) => "sig")
) ;let

(check-report)
