;; parse-openai-xml-tool-call
;; 从模型正文里解析 XML 形式的工具调用。
;;
;; 语法
;; ----
;; (parse-openai-xml-tool-call text)
;;
;; 参数
;; ----
;; text : string
;; 模型输出的正文。
;;
;; 返回值
;; ----
;; pair | #f
;; 解析出的 (工具名 . 参数 alist)；非 XML 形式时返回 #f。

(import (liii check)
  (liii pi ai api openai-completions tool-call-text)
) ;import

(check-set-mode! 'report-failed)

;; 标签内是 JSON：直接取 name 与 arguments
(check (parse-openai-xml-tool-call
         "<tool_call>{\"name\":\"bash\",\"arguments\":{\"command\":\"ls\"}}</tool_call>")
  =>
  (cons "bash" '(("command" . "ls")))
) ;check

;; 标签内是 function/parameter 形式：解析参数列表
(check (parse-openai-xml-tool-call
         "<tool_call><function=bash><parameter=command>ls -l</parameter></function></tool_call>")
  =>
  (cons "bash" '(("command" . "ls -l")))
) ;check

;; parameter 值为数字时转为数值
(check (parse-openai-xml-tool-call
         "<tool_call><function=bash><parameter=timeout>30</parameter></function></tool_call>")
  =>
  (cons "bash" '(("timeout" . 30)))
) ;check

;; 非 XML 形式返回 #f
(check (parse-openai-xml-tool-call "just plain text") => #f)
(check (parse-openai-xml-tool-call "{\"name\":\"bash\"}") => #f)

(check-report)
