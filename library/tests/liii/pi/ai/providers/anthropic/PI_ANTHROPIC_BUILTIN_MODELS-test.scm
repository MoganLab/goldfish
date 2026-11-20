;; PI_ANTHROPIC_BUILTIN_MODELS
;; Anthropic 内置模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_ANTHROPIC_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各内置模型配置信息的关联列表。

(import (liii check) (liii pi ai providers anthropic))

(check-set-mode! 'report-failed)

(check-true (pair? PI_ANTHROPIC_BUILTIN_MODELS))
(check
  (map (lambda (m) (cdr (assoc "id" m))) PI_ANTHROPIC_BUILTIN_MODELS)
  =>
  '("claude-opus-5-5" "claude-sonnet-5-5" "claude-haiku-5-5" "claude-fable-5-2")
) ;check

;; 所有模型：1M 上下文、128K 输出、推理模型、支持图片输入
(for-each
  (lambda (m)
    (check (cdr (assoc "contextWindow" m)) => 1000000)
    (check (cdr (assoc "maxTokens" m)) => 128000)
    (check (cdr (assoc "reasoning" m)) => #t)
    (check (cdr (assoc "input" m)) => '("text" "image"))
  ) ;lambda
  PI_ANTHROPIC_BUILTIN_MODELS
) ;for-each

(check-report)
