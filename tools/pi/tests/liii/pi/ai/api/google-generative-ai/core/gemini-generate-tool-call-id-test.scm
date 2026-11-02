;; gemini-generate-tool-call-id
;; 依据工具名、时间戳与计数器生成规范化的 Gemini 工具调用 id。
;;
;; 语法
;; ----
;; (gemini-generate-tool-call-id name timestamp counter)
;;
;; 参数
;; ----
;; name : string
;; 工具名。
;; timestamp : number
;; 生成时的时间戳。
;; counter : number
;; 同名工具的调用序号。
;;
;; 返回值
;; ----
;; string
;; 规范化后的工具调用 id，形如 <name>_<timestamp>_<counter>。

(import (liii check)
  (liii pi ai api google-generative-ai core)
) ;import

(check-set-mode! 'report-failed)

;; 常规组合
(check (gemini-generate-tool-call-id "bash" 1700000000 1) => "bash_1700000000_1")

;; 工具名中的非法字符被替换为下划线
(check (gemini-generate-tool-call-id "my/tool" 1 2) => "my_tool_1_2")

;; 计数器递增产生不同 id
(check (gemini-generate-tool-call-id "bash" 1 1) => "bash_1_1")
(check (gemini-generate-tool-call-id "bash" 1 2) => "bash_1_2")

;; 缺省参数支持（零参或缺省部分参数）
(check (gemini-generate-tool-call-id) => "call_0_1")
(check (gemini-generate-tool-call-id "bash") => "bash_0_1")
(check (gemini-generate-tool-call-id "bash" 100) => "bash_100_1")

(check-report)
