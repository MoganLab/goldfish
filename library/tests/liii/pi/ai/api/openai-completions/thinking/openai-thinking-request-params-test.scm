;; openai-thinking-request-params
;; 按模型条目与思考级别生成 OpenAI Chat Completions 请求侧思考字段。
;;
;; 语法
;; ----
;; (openai-thinking-request-params model level) -> alist
;;
;; 参数
;; ----
;; model : (or alist? #f)
;; 模型目录原始条目，读取 reasoning、thinkingLevelMap 与 compat。
;; level : (or string? #f)
;; 思考级别；非字符串或 "off" 视为关闭。
;;
;; 返回值
;; ----
;; alist
;; 要合并进请求体的字段；不支持思考时不注入任何字段。

(import (liii check) (liii pi ai api openai-completions thinking))

(check-set-mode! 'report-failed)

(define DEEPSEEK
  '(("id" . "m")
    ("reasoning" . true)
    ("thinkingLevelMap" ("low" . "low") ("medium" . null)
      ("high" . "high") ("max" . "max")
    ) ;thinkingLevelMap
    ("compat" ("thinkingFormat" . "deepseek"))
   ) ;list
) ;define

(define OPENAI
  '(("id" . "m")
    ("reasoning" . true)
    ("compat" ("thinkingFormat" . "openai"))
   ) ;list
) ;define

(define PLAIN
  '(("id" . "m") ("reasoning" . false))
) ;define

;; deepseek：开启思考并写 reasoning_effort
(check (openai-thinking-request-params DEEPSEEK "high")
  =>
  '(("thinking" ("type" . "enabled")) ("reasoning_effort" . "high"))
) ;check

;; deepseek：off 写 disabled，不写 reasoning_effort
(check (openai-thinking-request-params DEEPSEEK "off")
  =>
  '(("thinking" ("type" . "disabled")))
) ;check

;; openai：只写 reasoning_effort
(check (openai-thinking-request-params OPENAI "low")
  =>
  '(("reasoning_effort" . "low"))
) ;check

;; 不支持 reasoning 的模型不注入
(check (openai-thinking-request-params PLAIN "high") => '())

;; 模型条目缺失或级别为空时不注入
(check (openai-thinking-request-params #f "high") => '())
(check (openai-thinking-request-params OPENAI #f) => '())

(check-report)
