(import (liii check)
        (liii pi ai event) (liii pi ai models))

(check-set-mode! 'report-failed)

;; pi-load-models-json
;; 从 models.json 读取用户自定义模型目录，缺失时返回 '()，
;; 读取结果顶层键为字符串 "providers"（与 models-store.json 结构对齐）。
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"))

(define (%to-list v)
  (cond ((vector? v) (vector->list v))
        ((list? v) v)
        (else '())))

(let ((models-json (pi-load-models-json fixture-dir)))
  (check-true (pair? models-json))
  (let ((providers (cdr (assoc "providers" models-json))))
    (check-true (pair? providers))
    (check-true (pair? (assoc "deepseek" providers)))
    (check-true (pair? (assoc "glm" providers)))
    ;; provider 级配置含 baseUrl 与 models 列表
    (let* ((ds (cdr (assoc "deepseek" providers)))
           (models (%to-list (cdr (assoc "models" ds)))))
      (check (cdr (assoc "baseUrl" ds)) => "https://api.deepseek.com/anthropic")
      (check (length models) => 2)
      (let ((m1 (car models)))
        (check (cdr (assoc "id" m1)) => "deepseek-v4-flash")
        (check (cdr (assoc "contextWindow" m1)) => 1000000)
        (check (cdr (assoc "maxTokens" m1)) => 131072)))))

;; 文件缺失时返回 '()
(let ((models-json (pi-load-models-json "tests/fixtures")))
  (check models-json => '()))

(check-report)
