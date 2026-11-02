(import (scheme base) (liii check) (liii pi ai models list) (liii pi ai models))

(check-set-mode! 'report-failed)

;; 固定样本目录（不再依赖 models-store.json / models.json 读取）

(define sample-store
  (list
    (cons "google"
      (list
        (cons "models"
          (list (list (cons "id" "gemini-3.8-flash")
                  (cons "name" "Gemini 3.8 Flash")
                  (cons "contextWindow" 1048576)
                  (cons "maxTokens" 65536)
                ) ;list
            (list (cons "id" "gemini-2.5-pro")
              (cons "name" "Gemini 2.5 Pro")
              (cons "contextWindow" 2097152)
              (cons "maxTokens" 65536)
            ) ;list
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
    (cons "deepseek"
      (list
        (cons "models"
          (list (list (cons "id" "deepseek-v4-flash")
                  (cons "contextWindow" 1000000)
                  (cons "maxTokens" 131072)
                ) ;list
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
    (cons "kimi-coding"
      (list
        (cons "models"
          (list (list (cons "id" "kimi-for-coding")
                  (cons "contextWindow" 262144)
                  (cons "maxTokens" 32768)
                ) ;list
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
  ) ;list
) ;define

;; pi-list-models 过滤特定 provider
(let ((google-models (pi-list-models sample-store "google")))
  (check (length google-models) => 2)
  (let ((m1 (car google-models)))
    (check (cdr (assoc 'provider m1)) => "google")
    (check (cdr (assoc 'model m1)) => "gemini-2.5-pro")
    (check (cdr (assoc 'context m1)) => "2.1M")
    (check (cdr (assoc 'max-out m1)) => "65.5K")
    (check (cdr (assoc 'thinking m1)) => "no")
    (check (cdr (assoc 'images m1)) => "no")
  ) ;let
) ;let

;; pi-list-models 全部 provider（传 "all" 或 #f 或缺省）
(let ((all-models-str (pi-list-models sample-store "all"))
      (all-models-false (pi-list-models sample-store #f))
      (all-models-default (pi-list-models sample-store))
     ) ;
  (check (length all-models-str) => 4)
  (check (length all-models-false) => 4)
  (check (length all-models-default) => 4)
  ;; 排序：provider 名字典序 deepseek < google < kimi-coding
  (check (cdr (assoc 'provider (car all-models-str))) => "deepseek")
  (check (cdr (assoc 'provider (cadr all-models-str))) => "google")
  (check (cdr (assoc 'provider (cadddr all-models-str))) => "kimi-coding")
) ;let

;; pi-list-models 子序列模糊搜索（跨 provider）
(let ((filtered (pi-list-models sample-store #f "flash")))
  (check (length filtered) => 2)
  (check (cdr (assoc 'model (car filtered))) => "deepseek-v4-flash")
  (check (cdr (assoc 'model (cadr filtered))) => "gemini-3.8-flash")
) ;let

;; pi-list-models 未知 provider 明确报错
(check-catch 'runtime-error (pi-list-models sample-store "unknown-provider"))

;; 带 enabled 标记的 store：全量行返回、enabled 字段、置顶排序、组内字典序

(define store-with-enabled
  (list
    (cons "google"
      (list (cons "enabled" #t)
        (cons "models"
          (list (list (cons "id" "gemini-2.5-flash")
                  (cons "name" "Gemini 2.5 Flash")
                  (cons "enabled" #t)
                ) ;list
            (list (cons "id" "gemini-2.5-pro")
              (cons "name" "Gemini 2.5 Pro")
              (cons "enabled" #f)
            ) ;list
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
    (cons "deepseek"
      (list (cons "enabled" #t)
        (cons "models"
          (list (list (cons "id" "deepseek-v4-flash") (cons "enabled" #t))
            (list (cons "id" "deepseek-v4-pro") (cons "enabled" #f))
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
    (cons "minimax-cn"
      (list (cons "enabled" #f)
        (cons "models" (list (list (cons "id" "abab6.5s") (cons "enabled" #t))))
      ) ;list
    ) ;cons
  ) ;list
) ;define

(let ((all-rows (pi-list-models store-with-enabled)))
  ;; 1. 全量行返回（包含未激活行）
  (check (length all-rows) => 5)

  ;; 2. 每行均携带 enabled 字段
  (check-true (pair? (assoc 'enabled (list-ref all-rows 0))))
  (check-true (pair? (assoc 'enabled (list-ref all-rows 1))))
  (check-true (pair? (assoc 'enabled (list-ref all-rows 2))))
  (check-true (pair? (assoc 'enabled (list-ref all-rows 3))))
  (check-true (pair? (assoc 'enabled (list-ref all-rows 4))))

  ;; 3. 已激活行全部排在未激活行之前（置顶）
  (check (cdr (assoc 'enabled (list-ref all-rows 0))) => #t)
  (check (cdr (assoc 'enabled (list-ref all-rows 1))) => #t)
  (check (cdr (assoc 'enabled (list-ref all-rows 2))) => #f)
  (check (cdr (assoc 'enabled (list-ref all-rows 3))) => #f)
  (check (cdr (assoc 'enabled (list-ref all-rows 4))) => #f)

  ;; 4. 组内 provider、model 字典序保持
  ;; 已激活组：deepseek/deepseek-v4-flash < google/gemini-2.5-flash
  (check (cdr (assoc 'provider (list-ref all-rows 0))) => "deepseek")
  (check (cdr (assoc 'model (list-ref all-rows 0))) => "deepseek-v4-flash")
  (check (cdr (assoc 'provider (list-ref all-rows 1))) => "google")
  (check (cdr (assoc 'model (list-ref all-rows 1))) => "gemini-2.5-flash")

  ;; 未激活组：deepseek/deepseek-v4-pro < google/gemini-2.5-pro < minimax-cn/abab6.5s
  (check (cdr (assoc 'provider (list-ref all-rows 2))) => "deepseek")
  (check (cdr (assoc 'model (list-ref all-rows 2))) => "deepseek-v4-pro")
  (check (cdr (assoc 'provider (list-ref all-rows 3))) => "google")
  (check (cdr (assoc 'model (list-ref all-rows 3))) => "gemini-2.5-pro")
  (check (cdr (assoc 'provider (list-ref all-rows 4))) => "minimax-cn")
  (check (cdr (assoc 'model (list-ref all-rows 4))) => "abab6.5s")
) ;let

;; --provider 过滤在全量目录上正常工作（返回该 provider 的全部模型，含未激活）
(let ((google-rows (pi-list-models store-with-enabled "google")))
  (check (length google-rows) => 2)
  (check (cdr (assoc 'model (list-ref google-rows 0))) => "gemini-2.5-flash")
  (check (cdr (assoc 'enabled (list-ref google-rows 0))) => #t)
  (check (cdr (assoc 'model (list-ref google-rows 1))) => "gemini-2.5-pro")
  (check (cdr (assoc 'enabled (list-ref google-rows 1))) => #f)
) ;let

;; 未激活 provider 也能通过 --provider 罗列
(let ((minimax-rows (pi-list-models store-with-enabled "minimax-cn")))
  (check (length minimax-rows) => 1)
  (check (cdr (assoc 'model (list-ref minimax-rows 0))) => "abab6.5s")
  (check (cdr (assoc 'enabled (list-ref minimax-rows 0))) => #f)
) ;let

;; 搜索过滤在全量目录上照旧工作（命中含未激活行）
(let ((pro-rows (pi-list-models store-with-enabled #f "pro")))
  (check (length pro-rows) => 2)
  (check (cdr (assoc 'model (list-ref pro-rows 0))) => "deepseek-v4-pro")
  (check (cdr (assoc 'model (list-ref pro-rows 1))) => "gemini-2.5-pro")
) ;let


(check-report)
