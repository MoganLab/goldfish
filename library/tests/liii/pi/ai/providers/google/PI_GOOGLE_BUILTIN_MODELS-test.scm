;; PI_GOOGLE_BUILTIN_MODELS
;; Google Gemini 内置模型元数据定义列表。
;;
;; 语法
;; ----
;; PI_GOOGLE_BUILTIN_MODELS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 包含各内置模型配置信息的关联列表。

(import (liii check) (liii pi ai providers google))

(check-set-mode! 'report-failed)

(check-true (pair? PI_GOOGLE_BUILTIN_MODELS))
(check (length PI_GOOGLE_BUILTIN_MODELS) => 24)

(define (find-model models mid)
  (let loop
    ((rest models))
    (cond ((null? rest) #f)
          ((equal? (cdr (assoc "id" (car rest))) mid) (car rest))
          (else (loop (cdr rest)))
    ) ;cond
  ) ;let
) ;define

(let ((model (find-model PI_GOOGLE_BUILTIN_MODELS "gemini-4-hydrogen")))
  (check-true (pair? model))
  (check (cdr (assoc "name" model)) => "Gemini 4 Hydrogen")
  (check (cdr (assoc "baseUrl" model))
    =>
    "https://generativelanguage.googleapis.com/v1beta"
  ) ;check
  (check (cdr (assoc "contextWindow" model)) => 1048576)
  (check (cdr (assoc "maxTokens" model)) => 32768)
  (check (cdr (assoc "reasoning" model)) => #t)
  (check (cdr (assoc "input" model)) => '("text" "image"))
) ;let

(check-report)
