(import (liii check)
        (liii pi ai api anthropic-messages) (liii pi ai event) (liii pi ai api google-generative-ai) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

;; 多协议 toolResult 格式转换测试（支持 vector 与 list 两种形态的 content，防止恢复后落入空串 fallback）
(let* ((vec-msg (pi-tool-result-message "call_b1"
                  "bash"
                  (vector (pi-text-block "SECRET-1071"))
                  #f))
       (raw-vec-msg (pi-tool-result-message "call_r1"
                      "read"
                      '#((("type" . "text") ("text" . "FILE-CONTENT-1071")))
                      #f))
       (list-msg (pi-tool-result-message "call_l1" "read" (list (pi-text-block "LIST-1071")) #f)))

  ;; 1. Anthropic 协议转换
  (let* ((ant-vec (anthropic-build-messages (list vec-msg)))
         (ant-raw (anthropic-build-messages (list raw-vec-msg)))
         (ant-list (anthropic-build-messages (list list-msg))))
    (check (vector-length ant-vec) => 1)
    (let* ((turn (vector-ref ant-vec 0)) (blks (cdr (assoc "content" turn))))
      (check (vector-length blks) => 1)
      (check (cdr (assoc "type" (vector-ref blks 0))) => "tool_result")
      (check (cdr (assoc "tool_use_id" (vector-ref blks 0))) => "call_b1")
      (check (cdr (assoc "content" (vector-ref blks 0))) => "SECRET-1071"))
    (let* ((turn (vector-ref ant-raw 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "FILE-CONTENT-1071"))
    (let* ((turn (vector-ref ant-list 0)) (blks (cdr (assoc "content" turn))))
      (check (cdr (assoc "content" (vector-ref blks 0))) => "LIST-1071")))

  ;; 2. Kimi 协议转换
  (let* ((openai-vec (openai-build-messages (list vec-msg) #f))
         (openai-raw (openai-build-messages (list raw-vec-msg) #f))
         (openai-list (openai-build-messages (list list-msg) #f)))
    (check (vector-length openai-vec) => 1)
    (let ((m (vector-ref openai-vec 0)))
      (check (cdr (assoc "role" m)) => "tool")
      (check (cdr (assoc "tool_call_id" m)) => "call_b1")
      (check (cdr (assoc "content" m)) => "SECRET-1071"))
    (let ((m (vector-ref openai-raw 0)))
      (check (cdr (assoc "role" m)) => "tool")
      (check (cdr (assoc "tool_call_id" m)) => "call_r1")
      (check (cdr (assoc "content" m)) => "FILE-CONTENT-1071"))
    (let ((m (vector-ref openai-list 0)))
      (check (cdr (assoc "role" m)) => "tool")
      (check (cdr (assoc "tool_call_id" m)) => "call_l1")
      (check (cdr (assoc "content" m)) => "LIST-1071")))

  ;; 3. Gemini 协议转换
  (let* ((gem-vec (gemini-build-contents (list vec-msg)))
         (gem-raw (gemini-build-contents (list raw-vec-msg)))
         (gem-list (gemini-build-contents (list list-msg))))
    (check (vector-length gem-vec) => 1)
    (let* ((turn (vector-ref gem-vec 0))
           (parts (cdr (assoc "parts" turn)))
           (fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (resp (cdr (assoc "response" fr))))
      (check (cdr (assoc "name" fr)) => "bash")
      (check (cdr (assoc "id" fr)) => "call_b1")
      (check (cdr (assoc "output" resp)) => "SECRET-1071"))
    (let* ((turn (vector-ref gem-raw 0))
           (parts (cdr (assoc "parts" turn)))
           (fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (resp (cdr (assoc "response" fr))))
      (check (cdr (assoc "output" resp)) => "FILE-CONTENT-1071"))
    (let* ((turn (vector-ref gem-list 0))
           (parts (cdr (assoc "parts" turn)))
           (fr (cdr (assoc "functionResponse" (vector-ref parts 0))))
           (resp (cdr (assoc "response" fr))))
      (check (cdr (assoc "output" resp)) => "LIST-1071")))

  ;; 4. DeepSeek 复用 anthropic-messages 协议（见 providers 注册表），
  ;;    抽查确认 anthropic 消息构造兼容 vector 消息即可
  (let* ((ds (anthropic-build-messages (list vec-msg)))
         (blks (cdr (assoc "content" (vector-ref ds 0)))))
    (check (cdr (assoc "content" (vector-ref blks 0))) => "SECRET-1071")))

(check-report)
