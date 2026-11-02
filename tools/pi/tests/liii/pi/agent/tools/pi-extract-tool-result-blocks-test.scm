;; pi-extract-tool-result-blocks
;; 从会话消息列表末尾截取指定数量的工具结果，并转换为 toolResult 消息块向量。
;;
;; 语法
;; ----
;; (pi-extract-tool-result-blocks msgs count)
;;
;; 参数
;; ----
;; msgs : list?，包含历史消息及末尾工具结果的消息列表。
;; count : integer?，需要截取的末尾工具结果消息数量。
;;
;; 返回值
;; ----
;; vector?，由标准 pi-tool-result-message 构成的向量。

(import (scheme base) (liii check) (liii pi ai message) (liii pi agent tools))

(check-set-mode! 'report-failed)

;; 1. 从末尾截取两个工具结果块并转为向量
(let* ((m1 '((role . "user") (content . "hello")))
       (m2
         '((toolCallId . "call_1")
           (toolName . "bash")
           (content ((type . "text") (text . "output 1")))
           (isError . #f))
       ) ;m2
       (m3
         '(("toolCallId" . "call_2")
           ("toolName" . "read")
           ("content" (("type" . "text") ("text" . "output 2")))
           ("isError" . #t))
       ) ;m3
       (msgs (list m1 m2 m3))
       (vec (pi-extract-tool-result-blocks msgs 2))
      ) ;
  (check-true (vector? vec))
  (check (vector-length vec) => 2)
  (let ((r1 (vector-ref vec 0)) (r2 (vector-ref vec 1)))
    (check-true (pi-tool-result-message? r1))
    (check (pi-tool-result-message-tool-call-id r1) => "call_1")
    (check (pi-tool-result-message-tool-name r1) => "bash")
    (check (pi-tool-result-message-is-error r1) => #f)
    (check (pi-tool-result-message-tool-call-id r2) => "call_2")
    (check (pi-tool-result-message-is-error r2) => #t)
  ) ;let
) ;let*

;; 2. 截取数量为 0 时返回空向量
(let ((msgs (list '((role . "user")))))
  (check (pi-extract-tool-result-blocks msgs 0) => #())
) ;let

(check-report)
