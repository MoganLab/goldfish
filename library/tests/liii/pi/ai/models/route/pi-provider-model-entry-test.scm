;; pi-provider-model-entry
;; 从模型目录中按 provider 与 model-id 查询模型的原始目录条目。
;;
;; 语法
;; ----
;; (pi-provider-model-entry provider model-id models-store)
;;
;; 参数
;; ----
;; provider : (or string? symbol?)
;; 服务商标识。
;;
;; model-id : string?
;; 模型标识符。
;;
;; models-store : list?
;; 模型目录（providers.json 等加载后的 alist）。
;;
;; 返回值
;; ----
;; (or list? boolean?)
;; 命中返回模型条目 alist（含 "id"、"input" 等原始字段）；未命中返回 #f。

(import (liii check) (liii pi ai models))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-provider-model-entry))

(let ((store
        (list
          (cons "google"
            (list (cons "name" "Google")
              (cons "models"
                (vector
                  (list (cons "id" "gemini-3.8-flash")
                    (cons "input" (vector "text" "image"))
                  ) ;list
                ) ;vector
              ) ;cons
            ) ;list
          ) ;cons
          (cons "deepseek"
            (list (cons "name" "DeepSeek")
              (cons "models"
                (vector
                  (list (cons "id" "deepseek-chat")
                    (cons "input" (vector "text"))
                  ) ;list
                ) ;vector
              ) ;cons
            ) ;list
          ) ;cons
        ) ;list
      ) ;store
   ) ;
  ;; 1. 命中：返回含原始字段的条目
  (let ((entry (pi-provider-model-entry "google" "gemini-3.8-flash" store)))
    (check-true (pair? entry))
    (check (cdr (assoc "id" entry)) => "gemini-3.8-flash")
    (check (member "image" (vector->list (cdr (assoc "input" entry)))) => '("image"))
  ) ;let

  ;; 2. 未命中返回 #f
  (check (pi-provider-model-entry "google" "no-such-model" store) => #f)
  (check (pi-provider-model-entry "unknown" "gemini-3.8-flash" store) => #f)

  ;; 3. 与 pi-provider-has-model? 一致
  (check (pi-provider-has-model? "google" "gemini-3.8-flash" store) => #t)
  (check (pi-provider-has-model? "google" "no-such-model" store) => #f)
) ;let

(check-report)
