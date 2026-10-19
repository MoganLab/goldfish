(import (scheme base)
        (liii check)
        (liii pi models list)
        (liii pi models))

(check-set-mode! 'report-failed)

;; pi-format-token-count 测试
(check (pi-format-token-count 1000000) => "1M")
(check (pi-format-token-count 1048576) => "1.0M")
(check (pi-format-token-count 131072) => "131.1K")
(check (pi-format-token-count 65536) => "65.5K")
(check (pi-format-token-count 128000) => "128K")
(check (pi-format-token-count 32768) => "32.8K")
(check (pi-format-token-count 4096) => "4.1K")
(check (pi-format-token-count 500) => "500")

(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"))

(define sample-store (pi-load-models-store fixture-dir))

;; pi-list-models 过滤特定 provider
(let ((google-models (pi-list-models sample-store "google")))
  (check (length google-models) => 2)
  (let ((m1 (car google-models)))
    (check (cdr (assoc 'provider m1)) => "google")
    (check (cdr (assoc 'model m1)) => "gemini-2.5-pro")
    (check (cdr (assoc 'context m1)) => "2.1M")
    (check (cdr (assoc 'max-out m1)) => "65.5K")
    (check (cdr (assoc 'thinking m1)) => "no")
    (check (cdr (assoc 'images m1)) => "no")))

;; pi-list-models 全部 provider 与排序
(let ((all-models (pi-list-models sample-store "all")))
  (check (length all-models) => 4)
  ;; 排序：provider 名字典序 anthropic < deepseek < google
  (check (cdr (assoc 'provider (car all-models))) => "anthropic")
  (check (cdr (assoc 'provider (cadr all-models))) => "deepseek")
  (check (cdr (assoc 'provider (cadddr all-models))) => "google"))

;; pi-list-models 搜索词过滤
(let ((filtered (pi-list-models sample-store "all" "flash")))
  (check (length filtered) => 2)
  (check (cdr (assoc 'model (car filtered))) => "deepseek-flash")
  (check (cdr (assoc 'model (cadr filtered))) => "gemini-3.8-flash"))

;; pi-list-models 未知 provider 明确报错
(check-catch 'runtime-error (pi-list-models sample-store "unknown-provider"))

(check-report)
