(import (liii check)
        (scheme file)
        (liii pi models catalog))

(check-set-mode! 'report-failed)

;; pi-build-models-catalog
;; 合并目录构造：models.json > models-store.json > 内置目录

(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"))

;; 1. 不存在或空 agent-dir 时，退化为纯内置目录 (7 个内置 provider)
(let* ((empty-catalog (pi-build-models-catalog "non-existent-dir-037"))
       (names (pi-catalog-provider-names empty-catalog)))
  (check (length names) => 7)
  (check-true (if (member "anthropic" names) #t #f))
  (check-true (if (member "google" names) #t #f))
  (check-true (if (member "deepseek" names) #t #f))
  (check-true (if (member "kimi-coding" names) #t #f))
  (check-true (if (member "minimax" names) #t #f))
  (check-true (if (member "minimax-cn" names) #t #f))
  (check-true (if (member "zai-coding-cn" names) #t #f)))

;; 2. 使用 default fixture，验证三层合并与优先级覆盖
(let* ((catalog (pi-build-models-catalog fixture-dir))
       (names (pi-catalog-provider-names catalog)))
  ;; default fixture 包含 glm (来自 models.json)，因此共有 8 个 provider
  (check (length names) => 8)
  (check-true (if (member "glm" names) #t #f))

  ;; 检查 deepseek 模型的优先级覆盖：models.json 中的 maxTokens 覆盖内置值
  (let* ((ds-models (pi-catalog-provider-models catalog "deepseek"))
         (flash (assoc "deepseek-v4-flash"
                       (map (lambda (m) (cons (cdr (assoc "id" m)) m)) ds-models))))
    (check-true (pair? flash))
    ;; 来自 models.json 的 maxTokens 是 131072，而内置是 384000
    (check (cdr (assoc "maxTokens" (cdr flash))) => 131072)
    ;; models-store 中的 deepseek-flash 也应该被合并进来
    (check-true (if (member "deepseek-flash"
                            (map (lambda (m) (cdr (assoc "id" m))) ds-models))
                    #t
                    #f)))

  ;; 检查 glm 模型 (只在 models.json 中存在)
  (let ((glm-models (pi-catalog-provider-models catalog "glm")))
    (check-true (pair? glm-models))))

(check-report)
