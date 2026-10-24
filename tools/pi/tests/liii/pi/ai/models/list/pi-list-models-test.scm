(import (scheme base)
        (liii check)
        (liii pi ai models list)
        (liii pi ai models))

(check-set-mode! 'report-failed)

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

;; pi-list-models 全部 provider（传 "all" 或 #f 或缺省）
(let ((all-models-str (pi-list-models sample-store "all"))
      (all-models-false (pi-list-models sample-store #f))
      (all-models-default (pi-list-models sample-store)))
  (check (length all-models-str) => 4)
  (check (length all-models-false) => 4)
  (check (length all-models-default) => 4)
  ;; 排序：provider 名字典序 anthropic < deepseek < google
  (check (cdr (assoc 'provider (car all-models-str))) => "anthropic")
  (check (cdr (assoc 'provider (cadr all-models-str))) => "deepseek")
  (check (cdr (assoc 'provider (cadddr all-models-str))) => "google"))

;; pi-list-models 子序列模糊搜索（跨 provider）
(let ((filtered (pi-list-models sample-store #f "flash")))
  (check (length filtered) => 2)
  (check (cdr (assoc 'model (car filtered))) => "deepseek-flash")
  (check (cdr (assoc 'model (cadr filtered))) => "gemini-3.8-flash"))

;; pi-list-models 未知 provider 明确报错
(check-catch 'runtime-error (pi-list-models sample-store "unknown-provider"))

(check-report)
