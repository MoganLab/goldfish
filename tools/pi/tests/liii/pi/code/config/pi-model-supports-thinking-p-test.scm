(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; pi-model-supports-thinking?
;; 查询模型目录中的 reasoning 元数据，判断模型是否支持思考级别设置。
;;
;; 语法
;; ----
;; (pi-model-supports-thinking? models-store provider model-id) -> boolean
;;
;; 参数
;; ----
;; models-store : alist
;; provider 到模型列表的目录。
;; provider : string | symbol
;; 供应商名称。
;; model-id : string
;; 要查询的模型标识。
;;
;; 返回值
;; ----
;; boolean
;; 找到模型时返回其 reasoning 字段的布尔值；模型或元数据不存在时返回 #t。
;;
;; 示例
;; ----
;; (pi-model-supports-thinking? catalog "google" "gemini") => #t

(define catalog
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

(check (pi-model-supports-thinking? catalog "google" "thinking-model") => #t)
(check (pi-model-supports-thinking? catalog "google" "plain-model") => #f)
(check (pi-model-supports-thinking? catalog "google" "missing-model") => #t)
(check (pi-model-supports-thinking? catalog "missing-provider" "model") => #t)

(check-report)
