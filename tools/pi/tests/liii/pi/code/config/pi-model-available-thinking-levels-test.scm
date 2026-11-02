;; pi-model-available-thinking-levels
;; 按模型能力计算可用思考级别集：目录条目缺失时返回全集；
;; reasoning 为假的模型只支持 off。
;;
;; 语法
;; ----
;; (pi-model-available-thinking-levels models-store provider model-id) -> list
;;
;; 参数
;; ----
;; models-store : alist
;; provider 到模型目录的 alist。
;; provider : string
;; provider id。
;; model-id : string
;; 模型 id。
;;
;; 返回值
;; ----
;; list
;; 可用思考级别列表。

(import (liii check)
  (liii pi ai thinking)
  (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

(define CATALOG
  (list (cons "google"
          (list (cons "models"
                  (list (list (cons "id" "no-reasoning")
                          (cons "reasoning" #f)
                        ) ;list
                  ) ;list
                ) ;cons
          ) ;list
        ) ;cons
  ) ;list
) ;define

(check (pi-model-available-thinking-levels CATALOG "google" "no-reasoning")
  =>
  '("off")
) ;check
(check (pi-model-available-thinking-levels CATALOG "google" "missing")
  =>
  pi-thinking-levels
) ;check
(check (pi-model-available-thinking-levels '() "google" "missing")
  =>
  pi-thinking-levels
) ;check

(check-report)
