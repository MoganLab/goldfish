(import (liii check)
        (liii pi ai models builtin))

(check-set-mode! 'report-failed)

;; PI_PROVIDER_BUILTIN_MODELS
;; 各 provider 的默认内置模型定义列表 (7 provider / 60 模型)

(check-true (pair? PI_PROVIDER_BUILTIN_MODELS))
(check (length PI_PROVIDER_BUILTIN_MODELS) => 5)

(define (find-provider-models name)
  (let ((p (assoc name PI_PROVIDER_BUILTIN_MODELS)))
    (if p (cdr p) '())))

(define (has-model? models mid)
  (let loop ((rest models))
    (cond ((null? rest) #f)
          ((equal? (cdr (assoc "id" (car rest))) mid) #t)
          (else (loop (cdr rest))))))

;; 验证各 provider 及其模型数量（anthropic / glm 已移除）
(check (length (find-provider-models "deepseek")) => 3)
(check (length (find-provider-models "google")) => 23)
(check (length (find-provider-models "kimi-coding")) => 5)
(check (length (find-provider-models "minimax-cn")) => 4)
(check (length (find-provider-models "minimax-cn")) => 4)
(check (length (find-provider-models "zai-coding-cn")) => 10)

;; 验证关键模型存在
(check-true (has-model? (find-provider-models "deepseek") "deepseek-v4-pro"))
(check-true (has-model? (find-provider-models "google") "gemini-3.8-flash"))
(check-true (has-model? (find-provider-models "kimi-coding") "kimi-for-coding"))
(check-true (has-model? (find-provider-models "minimax-cn") "MiniMax-M3.1-Flash"))
(check-true (has-model? (find-provider-models "minimax-cn") "MiniMax-M3"))
(check-true (has-model? (find-provider-models "minimax-cn") "MiniMax-M3.1-Flash"))
(check-true (has-model? (find-provider-models "minimax-cn") "MiniMax-M3"))
(check-true (has-model? (find-provider-models "zai-coding-cn") "glm-5.3"))

(check-report)
