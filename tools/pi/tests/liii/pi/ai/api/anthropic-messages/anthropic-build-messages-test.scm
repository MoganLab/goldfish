(import (liii check) (liii json) (liii string) (liii pi ai api anthropic-messages) (liii pi ai event) (liii pi ai message))

(check-set-mode! 'report-failed)

;; anthropic-build-messages
;; 将内部会话消息历史转换为 Anthropic Messages API 消息。
;;
;; 语法
;; ----
;; (anthropic-build-messages messages)
;;
;; 参数
;; ----
;; messages : list
;; 内部 user、assistant 和 toolResult 消息列表，支持 alist 和 record。
;;
;; 返回值
;; ----
;; vector
;; Anthropic API messages 向量，每个元素为包含 role 和 content 的 alist。
;;
;; 说明
;; ----
;; 连续 toolResult 合并为一个 user message，未知角色被过滤。
;; 工具调用转换为 tool_use，空或畸形参数规范化为 JSON 对象。
;;
;; 示例
;; ----
;; (anthropic-build-messages (list (pi-user-message "hello"))) => #((("role" . "user") ...))
(let* ((messages (list (list (cons 'role "user") (cons 'content "hello"))
                       (list (cons 'role "assistant")
                             (cons 'content (list (list (cons 'type "text") (cons 'text "Let me check"))
                                                  (list (cons 'type "toolCall")
                                                        (cons 'id "call_1")
                                                        (cons 'name "bash")
                                                        (cons 'arguments '(("command" . "ls")))))))
                       (list (cons 'role "toolResult")
                             (cons 'toolCallId "call_1")
                             (cons 'toolName "bash")
                             (cons 'content (list (list (cons 'type "text") (cons 'text "file.txt"))))
                             (cons 'isError #f))))
       (ant-msgs (anthropic-build-messages messages)))
  (check (vector-length ant-msgs) => 3)
  (let ((u-msg (vector-ref ant-msgs 0))
        (a-msg (vector-ref ant-msgs 1))
        (t-msg (vector-ref ant-msgs 2)))
    (check (cdr (assoc "role" u-msg)) => "user")
    (check (cdr (assoc "role" a-msg)) => "assistant")
    (let ((blks (cdr (assoc "content" a-msg))))
      (check (vector-length blks) => 2)
      (check (cdr (assoc "type" (vector-ref blks 0))) => "text")
      (check (cdr (assoc "type" (vector-ref blks 1))) => "tool_use")
      (check (cdr (assoc "id" (vector-ref blks 1))) => "call_1")
      (check (cdr (assoc "name" (vector-ref blks 1))) => "bash"))
    (check (cdr (assoc "role" t-msg)) => "user")
    (let ((tr-blks (cdr (assoc "content" t-msg))))
      (check (vector-length tr-blks) => 1)
      (check (cdr (assoc "type" (vector-ref tr-blks 0))) => "tool_result")
      (check (cdr (assoc "tool_use_id" (vector-ref tr-blks 0))) => "call_1")
      (check (cdr (assoc "content" (vector-ref tr-blks 0))) => "file.txt"))))

;; 支持 record 结构（强类型消息与 content block）
(let* ((messages (list (pi-user-message "hello record")
                       (pi-assistant-message
                         (list (pi-text-block "Let me check")
                               (pi-tool-call-block "call_1" "bash" '(("command" . "ls")))))
                       (pi-tool-result-message "call_1" "bash" (list (pi-text-block "file.txt")) #f)))
       (ant-msgs (anthropic-build-messages messages)))
  (check (vector-length ant-msgs) => 3)
  (let ((u-msg (vector-ref ant-msgs 0))
        (a-msg (vector-ref ant-msgs 1))
        (t-msg (vector-ref ant-msgs 2)))
    (check (cdr (assoc "role" u-msg)) => "user")
    (check (cdr (assoc "role" a-msg)) => "assistant")
    (let ((blks (cdr (assoc "content" a-msg))))
      (check (vector-length blks) => 2)
      (check (cdr (assoc "type" (vector-ref blks 0))) => "text")
      (check (cdr (assoc "type" (vector-ref blks 1))) => "tool_use")
      (check (cdr (assoc "id" (vector-ref blks 1))) => "call_1")
      (check (cdr (assoc "name" (vector-ref blks 1))) => "bash"))
    (check (cdr (assoc "role" t-msg)) => "user")
    (let ((tr-blks (cdr (assoc "content" t-msg))))
      (check (vector-length tr-blks) => 1)
      (check (cdr (assoc "type" (vector-ref tr-blks 0))) => "tool_result")
      (check (cdr (assoc "tool_use_id" (vector-ref tr-blks 0))) => "call_1")
      (check (cdr (assoc "content" (vector-ref tr-blks 0))) => "file.txt"))))

;; 3. 各种边界形态的 tool_use 参数序列化防回归（空参数、字符串、畸形 entry）
(let* ((empty-blk (pi-tool-call-block "c_empty" "ls" '()))
       (empty-obj-blk (pi-tool-call-block "c_empty_obj" "ls" '(())))
       (str-json-blk (pi-tool-call-block "c_str_json" "bash" "{\"command\":\"pwd\"}"))
       (str-empty-blk (pi-tool-call-block "c_str_empty" "ls" "{}"))
       (malformed-blk (pi-tool-call-block "c_malformed" "read" '(("path"))))
       (msg (pi-assistant-message (list empty-blk
                                        empty-obj-blk
                                        str-json-blk
                                        str-empty-blk
                                        malformed-blk)))
       (ant-msgs (anthropic-build-messages (list msg)))
       (body (anthropic-build-request-body "claude-test" ant-msgs "" '() 1024))
       (json-str (json->string body)))
  (check-true (string? json-str))
  (check-true (string-contains? json-str "\"input\":{}"))
  (check-true (string-contains? json-str "\"input\":{\"command\":\"pwd\"}"))
  (check-true (string-contains? json-str "\"input\":{\"path\":{}}")))

;; 连续 toolResult 合并为一个 user message，未知角色被过滤
(let* ((messages (list (pi-user-message "run")
                       (pi-tool-result-message "c1" "read" (list (pi-text-block "one")) #f)
                       (pi-tool-result-message "c2" "bash" (list (pi-text-block "two")) #f)
                       '((role . "unknown") (content . "ignored"))))
       (ant-msgs (anthropic-build-messages messages))
       (merged (vector-ref ant-msgs 1))
       (blocks (cdr (assoc "content" merged))))
  (check (vector-length ant-msgs) => 2)
  (check (vector-length blocks) => 2)
  (check (cdr (assoc "tool_use_id" (vector-ref blocks 0))) => "c1")
  (check (cdr (assoc "tool_use_id" (vector-ref blocks 1))) => "c2"))

(check-report)
