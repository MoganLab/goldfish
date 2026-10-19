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
  (check (length rows) => 4)
  ;; pi-list-models 按 model 名字典序排序
  (check (map (lambda (r) (cdr (assoc 'model r))) rows)
    => '("glm-4.5-air" "glm-5-turbo" "glm-5.3" "glm-5.3-flash")))

;; glm-5.3 元数据正确（1M 上下文、131.1K max-out、thinking）
;; 搜索模式为子串匹配（"glm-5.3" 同时命中 glm-5.3 与 glm-5.3-flash）
(let ((rows (pi-list-models merged "glm" "glm-5.3")))
  (check (length rows) => 2)
  (let ((row (car rows)))
    (check (cdr (assoc 'provider row)) => "glm")
    (check (cdr (assoc 'model row)) => "glm-5.3")
    (check (cdr (assoc 'context row)) => "1M")
    (check (cdr (assoc 'max-out row)) => "131.1K")
    (check (cdr (assoc 'thinking row)) => "yes")))

;; glm-5-turbo 元数据正确（200K 上下文）
(let ((rows (pi-list-models merged "glm" "glm-5-turbo")))
  (check (length rows) => 1)
  (let ((row (car rows)))
    (check (cdr (assoc 'context row)) => "200K")
    (check (cdr (assoc 'max-out row)) => "131.1K")))

;; 合并后 pi-resolve-model-metadata 可命中 glm-5.3 系列的正确元数据
(let ((meta (pi-resolve-model-metadata "glm" "glm-5.3" merged '())))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

(let ((meta (pi-resolve-model-metadata "glm" "glm-5.3-flash" merged '())))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

(let ((meta (pi-resolve-model-metadata "glm" "glm-5-turbo" merged '())))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 200000)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

(let ((meta (pi-resolve-model-metadata "glm" "glm-4.5-air" merged '())))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 128000)
  (check (cdr (assoc 'max-tokens meta)) => 16384))

;; 合并后 pi-resolve-model-metadata 可命中 deepseek-v4-flash 的正确元数据
(let ((meta (pi-resolve-model-metadata "deepseek" "deepseek-v4-flash" merged '())))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

;; 空 models-json 合并不改变 store
(let ((merged-empty (pi-merge-models-json-into-store models-store '())))
  (check merged-empty => models-store))

(check-report)
