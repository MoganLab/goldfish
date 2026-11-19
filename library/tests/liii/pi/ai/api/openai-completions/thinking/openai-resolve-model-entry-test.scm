;; openai-resolve-model-entry
;; 从 models-store 中按 provider/model id 取原始模型条目。
;;
;; 语法
;; ----
;; (openai-resolve-model-entry models-store provider model-id) -> alist | #f
;;
;; 参数
;; ----
;; models-store : alist
;; provider 到模型列表的目录。
;; provider : string
;; provider id。
;; model-id : string
;; 模型 id。
;;
;; 返回值
;; ----
;; alist | #f
;; 命中时返回模型条目，否则返回 #f。

(import (liii check) (liii pi ai api openai-completions thinking))

(check-set-mode! 'report-failed)

(define STORE
  '(("google"
     ("models" (("id" . "m1") ("reasoning" . true))
       (("id" . "m2") ("reasoning" . false))
     ) ;models
    ) ;list
   ) ;list
) ;define

(check (cdr (assoc "reasoning" (openai-resolve-model-entry STORE "google" "m1")))
  =>
  'true
) ;check
(check (cdr (assoc "reasoning" (openai-resolve-model-entry STORE "google" "m2")))
  =>
  'false
) ;check
(check (openai-resolve-model-entry STORE "google" "missing") => #f)
(check (openai-resolve-model-entry STORE "missing" "m1") => #f)

(check-report)
