;; gemini-generate-tool-call-id
;; 自动生成合规的工具调用 ID。
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

(import (liii check) (liii pi ai api google-generative-ai))

(check-set-mode! 'report-failed)

(check (gemini-generate-tool-call-id "bash" 1700000000 1) => "bash_1700000000_1")

(check-report)
