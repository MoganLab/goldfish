;; parse-openai-text-tool-call
;; 从模型正文里解析工具调用（优先 XML 形式，其次 JSON 形式）。
;;
;; 语法
;; ----
;; (parse-openai-text-tool-call text)
;;
;; 参数
;; ----
;; text : string
;; 模型输出的正文。
;;
;; 返回值
;; ----
;; pair | #f
;; 解析出的 (工具名 . 参数 alist)；没有工具调用时返回 #f。

(import (liii check)
  (liii pi ai api openai-completions tool-call-text)
) ;import

(check-set-mode! 'report-failed)

;; XML 形式优先
(check (parse-openai-text-tool-call
         "<tool_call><function=bash><parameter=command>ls</parameter></function></tool_call>")
  =>
  (cons "bash" '(("command" . "ls")))
) ;check

;; JSON 形式：arguments 已是 alist
(check (parse-openai-text-tool-call
         "{\"name\":\"bash\",\"arguments\":{\"command\":\"ls\"}}")
  =>
  (cons "bash" '(("command" . "ls")))
) ;check

;; JSON 形式：arguments 是字符串时解析为 alist
(check (parse-openai-text-tool-call
         "{\"name\":\"bash\",\"arguments\":\"{\\\"command\\\":\\\"ls\\\"}\"}")
  =>
  (cons "bash" '(("command" . "ls")))
) ;check

;; bash 工具的字符串参数归一为 command
(check (parse-openai-text-tool-call
         "{\"name\":\"bash\",\"arguments\":\"ls -l\"}")
  =>
  (cons "bash" '(("command" . "ls -l")))
) ;check

;; 非字符串输入返回 #f
(check (parse-openai-text-tool-call #f) => #f)

;; 没有工具调用返回 #f
(check (parse-openai-text-tool-call "hello world") => #f)

(check-report)
