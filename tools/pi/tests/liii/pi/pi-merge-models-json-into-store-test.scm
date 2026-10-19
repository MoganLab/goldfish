(import (liii check)
        (liii list)
        (liii pi))

(check-set-mode! 'report-failed)

;; pi-merge-models-json-into-store
;; 将 models.json 中的用户自定义模型目录合并进 models-store：
;; 1) 新增 provider（如 glm）整体并入；
;; 2) 已有 provider（如 deepseek）补充缺失模型条目；
;; 3) 已存在的模型条目以 models-store 为准，不被覆盖。
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"))

(define models-store (pi-load-models-store fixture-dir))
(define models-json (pi-load-models-json fixture-dir))
(define merged (pi-merge-models-json-into-store models-store models-json))

;; 合并后 glm 出现在目录中
(check-true (pair? (assoc "glm" merged)))

;; deepseek 新增 deepseek-v4-flash，原有模型保留
(let* ((ds (cdr (assoc "deepseek" merged)))
       (models (cdr (assoc "models" ds)))
       (ids (map (lambda (m) (cdr (assoc "id" m))) models)))
  (check-true (pair? (member "deepseek-v4-flash" ids)))
  (check-true (pair? (member "deepseek-v4-pro" ids)))
  (check-true (pair? (member "deepseek-flash" ids))))

;; 已存在的 deepseek-flash 条目以 models-store 为准（contextWindow 为 store 值）
(let* ((ds (cdr (assoc "deepseek" merged)))
       (models (cdr (assoc "models" ds)))
       (flash (car (filter (lambda (m) (string=? (cdr (assoc "id" m)) "deepseek-flash"))
                     models))))
  (check (cdr (assoc "contextWindow" flash)) => 1048576))

;; 合并后 pi-list-models 能列出 deepseek-v4-flash（含正确元数据）与 glm 模型
(let ((rows (pi-list-models merged "deepseek" "deepseek-v4-flash")))
  (check (length rows) => 1)
  (let ((row (car rows)))
    (check (cdr (assoc 'provider row)) => "deepseek")
    (check (cdr (assoc 'model row)) => "deepseek-v4-flash")
    (check (cdr (assoc 'context row)) => "1M")
    (check (cdr (assoc 'max-out row)) => "131.1K")
    (check (cdr (assoc 'thinking row)) => "yes")))

(let ((rows (pi-list-models merged "glm")))
  (check (length rows) => 1)
  (check (cdr (assoc 'model (car rows))) => "glm-5.3"))

;; 合并后 pi-resolve-model-metadata 可命中 deepseek-v4-flash 的正确元数据
(let ((meta (pi-resolve-model-metadata "deepseek" "deepseek-v4-flash" merged '())))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

;; 空 models-json 合并不改变 store
(let ((merged-empty (pi-merge-models-json-into-store models-store '())))
  (check merged-empty => models-store))

(check-report)
