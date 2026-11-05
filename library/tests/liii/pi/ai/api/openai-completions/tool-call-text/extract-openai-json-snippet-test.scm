;; extract-openai-json-snippet
;; 从模型正文里截取可能的 JSON 片段（XML 标签 / 代码块 / name 字段）。
;;
;; 语法
;; ----
;; (extract-openai-json-snippet text)
;;
;; 参数
;; ----
;; text : string
;; 模型输出的正文。
;;
;; 返回值
;; ----
;; string
;; 截取到的 JSON 片段；没有可用片段时返回去除首尾空白后的原文。

(import (liii check)
  (liii string)
  (liii pi ai api openai-completions tool-call-text)
) ;import

(check-set-mode! 'report-failed)

;; XML 工具调用标签包裹的片段
(check (extract-openai-json-snippet
         "<tool_call>{\"name\":\"bash\"}</tool_call>")
  =>
  "{\"name\":\"bash\"}"
) ;check

;; json 代码块
(check (extract-openai-json-snippet
         "```json\n{\"name\":\"bash\"}\n```")
  =>
  "{\"name\":\"bash\"}"
) ;check

;; 通用代码块
(check (extract-openai-json-snippet
         "```\n{\"name\":\"bash\"}\n```")
  =>
  "{\"name\":\"bash\"}"
) ;check

;; 裸 JSON 片段：从 name 字段到最后一个右花括号
(check (extract-openai-json-snippet
         "prefix {\"name\":\"bash\",\"command\":\"ls\"} suffix")
  =>
  "{\"name\":\"bash\",\"command\":\"ls\"}"
) ;check

;; 没有 JSON 片段时返回去空白后的原文
(check (extract-openai-json-snippet "  hello  ") => "hello")

(check-report)
