(import (scheme base)
        (liii check)
        (liii pi ai models list)
        (liii pi ai models))

(check-set-mode! 'report-failed)

;; 固定样本目录（不再依赖 models-store.json / models.json 读取）
(define sample-store
  (list
    (cons "google"
      (list (cons "models"
              (list (list (cons "id" "gemini-3.8-flash")
                          (cons "name" "Gemini 3.8 Flash")
                          (cons "contextWindow" 1048576)
                          (cons "maxTokens" 65536))
                    (list (cons "id" "gemini-2.5-pro")
                          (cons "name" "Gemini 2.5 Pro")
                          (cons "contextWindow" 2097152)
                          (cons "maxTokens" 65536))))))
    (cons "deepseek"
      (list (cons "models"
              (list (list (cons "id" "deepseek-v4-flash")
                          (cons "contextWindow" 1000000)
                          (cons "maxTokens" 131072))))))
    (cons "kimi-coding"
      (list (cons "models"
              (list (list (cons "id" "kimi-for-coding")
                          (cons "contextWindow" 262144)
                          (cons "maxTokens" 32768))))))))

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
  ;; 排序：provider 名字典序 deepseek < google < kimi-coding
  (check (cdr (assoc 'provider (car all-models-str))) => "deepseek")
  (check (cdr (assoc 'provider (cadr all-models-str))) => "google")
  (check (cdr (assoc 'provider (cadddr all-models-str))) => "kimi-coding"))

;; pi-list-models 子序列模糊搜索（跨 provider）
(let ((filtered (pi-list-models sample-store #f "flash")))
  (check (length filtered) => 2)
  (check (cdr (assoc 'model (car filtered))) => "deepseek-v4-flash")
  (check (cdr (assoc 'model (cadr filtered))) => "gemini-3.8-flash"))

;; pi-list-models 未知 provider 明确报错
(check-catch 'runtime-error (pi-list-models sample-store "unknown-provider"))

(check-report)
