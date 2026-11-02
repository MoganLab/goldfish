;; openai-stream-state-blocks-set!
;; 设置 OpenAI 流状态中的消息块列表。
;;
;; 语法
;; ----
;; (openai-stream-state-blocks-set! state value)
;;
;; 参数
;; ----
;; state : openai-stream-state | vector
;; 流状态；record 为当前表示，vector 用于兼容旧调用。
;; value : list
;; 已完成的 pi 消息块列表。
;;
;; 返回值
;; ----
;; 未指定
;; 通过副作用更新流状态。

;; 说明
;; ----
;; record 状态直接更新记录字段；vector 状态更新对应的下标。

(import (liii check)
  (liii pi ai api openai-completions stream-state)
) ;import

(check-set-mode! 'report-failed)

;; record 状态
(let ((state (make-openai-stream-state '() #f '() "stop" #f '())))
  (openai-stream-state-blocks-set! state '())
  (check (openai-stream-state-blocks state) => '())
) ;let*

;; vector 状态（兼容旧调用）
(let ((state (vector '() #f '() "stop" #f '())))
  (openai-stream-state-blocks-set! state '())
  (check (openai-stream-state-blocks state) => '())
) ;let*

(check-report)
