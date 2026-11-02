;; pi-make-tool-result-msg
;; 根据工具调用 ID、名称、文本内容与错误标识构造标准的 toolResult 消息对象。
;;
;; 语法
;; ----
;; (pi-make-tool-result-msg id name text is-err)
;;
;; 参数
;; ----
;; id : string?，对应工具调用的唯一标识 ID。
;; name : string?，工具名称（如 "bash"）。
;; text : string?，工具输出的文本结果。
;; is-err : boolean?，是否执行失败报错。
;;
;; 返回值
;; ----
;; pi-tool-result-message?，构造出的 toolResult 消息 record 对象。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 构造成功的工具结果消息
(let ((msg (pi-make-tool-result-msg "call_1" "bash" "file.txt\n" #f)))
  (check-true (pi-tool-result-message? msg))
  (check (pi-tool-result-message-tool-call-id msg) => "call_1")
  (check (pi-tool-result-message-tool-name msg) => "bash")
  (check (pi-tool-result-message-is-error msg) => #f)
  (let ((content (pi-tool-result-message-content msg)))
    (check-true (pair? content))
    (check (pi-text-block-text (car content)) => "file.txt\n")
  ) ;let
) ;let

;; 2. 构造失败报错的工具结果消息
(let ((msg (pi-make-tool-result-msg "call_2" "read" "file not found" #t)))
  (check-true (pi-tool-result-message? msg))
  (check (pi-tool-result-message-is-error msg) => #t)
  (check (pi-text-block-text (car (pi-tool-result-message-content msg)))
    =>
    "file not found"
  ) ;check
) ;let

(check-report)
