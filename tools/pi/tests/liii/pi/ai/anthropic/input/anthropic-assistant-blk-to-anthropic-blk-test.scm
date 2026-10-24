(import (liii check)
  (liii json)
  (liii pi ai anthropic input)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; anthropic-assistant-blk->anthropic-blk
;; 将 assistant message 中的 block 转换为 Anthropic API 所需的 tool_use / text / thinking 格式。

;; 1. text 块转换
(let* ((blk (pi-text-block "hello world"))
       (ant-blk (anthropic-assistant-blk->anthropic-blk blk)))
  (check (cdr (assoc "type" ant-blk)) => "text")
  (check (cdr (assoc "text" ant-blk)) => "hello world"))

;; 2. toolCall 块正常参数转换
(let* ((blk (pi-tool-call-block "call_1" "bash" '(("command" . "ls"))))
       (ant-blk (anthropic-assistant-blk->anthropic-blk blk)))
  (check (cdr (assoc "type" ant-blk)) => "tool_use")
  (check (cdr (assoc "id" ant-blk)) => "call_1")
  (check (cdr (assoc "name" ant-blk)) => "bash")
  (check (cdr (assoc "command" (cdr (assoc "input" ant-blk)))) => "ls")
  (check (json->string ant-blk)
    => "{\"type\":\"tool_use\",\"id\":\"call_1\",\"name\":\"bash\",\"input\":{\"command\":\"ls\"}}"))

;; 3. toolCall 块空参数转换（核心防回归）：input 必须是 (()) 从而序列化为 {}，绝不能退化为 ("input")
(let* ((blk (pi-tool-call-block "call_empty" "ls" '()))
       (ant-blk (anthropic-assistant-blk->anthropic-blk blk)))
  (check (cdr (assoc "type" ant-blk)) => "tool_use")
  (check (cdr (assoc "id" ant-blk)) => "call_empty")
  (check (cdr (assoc "name" ant-blk)) => "ls")
  (check (cdr (assoc "input" ant-blk)) => '(()))
  (check (json->string ant-blk)
    => "{\"type\":\"tool_use\",\"id\":\"call_empty\",\"name\":\"ls\",\"input\":{}}"))

;; 4. thinking 块转换（含 signature 与无 signature）
(let* ((blk1 (pi-thinking-block "pondering" "sig_abc"))
       (ant-blk1 (anthropic-assistant-blk->anthropic-blk blk1))
       (blk2 (pi-thinking-block "pure-thought"))
       (ant-blk2 (anthropic-assistant-blk->anthropic-blk blk2)))
  (check (cdr (assoc "type" ant-blk1)) => "thinking")
  (check (cdr (assoc "thinking" ant-blk1)) => "pondering")
  (check (cdr (assoc "signature" ant-blk1)) => "sig_abc")
  (check (cdr (assoc "type" ant-blk2)) => "thinking")
  (check (cdr (assoc "thinking" ant-blk2)) => "pure-thought")
  (check (assoc "signature" ant-blk2) => #f))

(check-report)
