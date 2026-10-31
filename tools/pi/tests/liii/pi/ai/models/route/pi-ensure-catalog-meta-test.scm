(import (liii check)
        (liii pi ai models)
        (liii pi ai config record))

(check-set-mode! 'report-failed)

;; 内置目录（含 models 包裹结构），供可用性判定使用
(define BUILTIN-CATALOG (pi-builtin-augment-all '()))

;; pi-ensure-catalog-meta
;; 确保已被识别为内置模型的元数据 matched? 字段标记为 #t

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
      "tests/fixtures/default"
      "tools/pi/tests/fixtures/default"))

(define fixture-store (pi-load-models-store FIXTURE_DIR))

;; 1. 当 meta 未匹配，但属于内置模型时，提升 matched? 为 #t
(let* ((raw-meta (pi-resolve-model-metadata "kimi-coding" "k3" fixture-store))
       (ensured-meta (pi-ensure-catalog-meta raw-meta "kimi-coding" "k3" BUILTIN-CATALOG)))
  (check (pi-model-metadata-id ensured-meta) => "k3")
  (check (pi-model-metadata-provider ensured-meta) => "kimi-coding")
  (check-true (pi-model-metadata-matched? ensured-meta)))

;; 2. 当 meta 已经匹配时，保持原样
(let* ((raw-meta (pi-resolve-model-metadata "google" "gemini-3.8-flash" fixture-store))
       (ensured-meta (pi-ensure-catalog-meta raw-meta "google" "gemini-3.8-flash" fixture-store)))
  (check-true (pi-model-metadata-matched? ensured-meta)))

;; 3. 当模型既不在 models-store 也不在内置模型时，保持 matched? 为 #f
(let* ((raw-meta (pi-resolve-model-metadata "google" "non-existent-model" fixture-store))
       (ensured-meta (pi-ensure-catalog-meta raw-meta "google" "non-existent-model" fixture-store)))
  (check-false (pi-model-metadata-matched? ensured-meta)))

(check-report)
