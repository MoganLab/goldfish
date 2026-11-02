;; pi-model-available-thinking-levels
;; 按模型目录中的 reasoning 元数据返回可用思考级别集。
;;
;; 语法
;; ----
;; (pi-model-available-thinking-levels models-store provider model-id)
;;
;; 参数
;; ----
;; models-store : alist
;; provider 到模型条目的模型目录。
;; provider : string | symbol
;; 供应商名称。
;; model-id : string
;; 模型标识。
;;
;; 返回值
;; ----
;; list
;; 可用思考级别字符串列表；模型元数据缺失时返回级别全集。

(import (liii check) (liii pi ai thinking) (liii pi code config))

(check-set-mode! 'report-failed)

(define CATALOG
  (list
    (cons "google"
      (list
        (cons "models"
          (list (list (cons "id" "thinking-model") (cons "reasoning" #t))
            (list (cons "id" "plain-model") (cons "reasoning" #f))
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
  ) ;list
) ;define

;; 支持 reasoning 的模型：未显式映射的 xhigh/max 不可用
(check (pi-model-available-thinking-levels CATALOG "google" "thinking-model")
  =>
  '("off" "minimal" "low" "medium" "high")
) ;check

;; 不支持 reasoning 的模型只支持 off
(check (pi-model-available-thinking-levels CATALOG "google" "plain-model")
  =>
  '("off")
) ;check

;; 模型或 provider 元数据缺失时返回级别全集
(check (pi-model-available-thinking-levels CATALOG "google" "missing-model")
  =>
  pi-thinking-levels
) ;check
(check (pi-model-available-thinking-levels '() "google" "missing-model")
  =>
  pi-thinking-levels
) ;check

(check-report)
