;; PI_PROVIDER_BUILTIN_MODELS
;; 各 provider 的默认内置模型定义列表。
;;
;; 语法
;; ----
;; PI_PROVIDER_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各 provider 名称及其模型定义的关联列表。

(import (liii check) (liii pi ai providers registry))

(check-set-mode! 'report-failed)

(check-true (pair? PI_PROVIDER_BUILTIN_MODELS))
(check (length PI_PROVIDER_BUILTIN_MODELS) => 8)

(define (find-provider-models name)
  (let ((p (assoc name PI_PROVIDER_BUILTIN_MODELS)))
    (if p (cdr p) '())
  ) ;let
) ;define

(define (has-model? models mid)
  (let loop
    ((rest models))
    (cond ((null? rest) #f)
          ((equal? (cdr (assoc "id" (car rest))) mid) #t)
          (else (loop (cdr rest)))
    ) ;cond
  ) ;let
) ;define

;; 验证各 provider 及其模型数量
(check (length (find-provider-models "deepseek")) => 3)
(check (length (find-provider-models "google")) => 23)
(check (length (find-provider-models "kimi-coding")) => 5)
(check (length (find-provider-models "minimax-cn")) => 4)
(check (length (find-provider-models "ollama")) => 3)
(check (length (find-provider-models "zai-coding-cn")) => 10)
(check (length (find-provider-models "opencode")) => 27)
(check (length (find-provider-models "all-in-one")) => 2)

;; 验证关键模型存在
(check-true (has-model? (find-provider-models "all-in-one") "coding-fast"))
(check-true (has-model? (find-provider-models "all-in-one") "coding-high-quality"))
(check-true (has-model? (find-provider-models "deepseek") "deepseek-v4-pro"))
(check-true (has-model? (find-provider-models "google") "gemini-3.8-flash"))
(check-true (has-model? (find-provider-models "kimi-coding") "kimi-for-coding"))
(check-true (has-model? (find-provider-models "minimax-cn") "MiniMax-M3.1-Flash")
) ;check-true
(check-true (has-model? (find-provider-models "minimax-cn") "MiniMax-M3"))
(check-true (has-model? (find-provider-models "ollama") "qwen2.5-coder:latest"))
(check-true (has-model? (find-provider-models "zai-coding-cn") "glm-5.3"))
(check-true (has-model? (find-provider-models "opencode") "deepseek-v4.1-flash"))

(check-report)
