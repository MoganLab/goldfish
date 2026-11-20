;; PI_DEFAULT_MODEL_COSTS
;; 默认模型费率定价表。
;;
;; 语法
;; ----
;; PI_DEFAULT_MODEL_COSTS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 各服务商内置模型的费率关联列表。

(import (scheme base) (liii check) (liii pi ai cost))

(check-set-mode! 'report-failed)

(check-true (pair? PI_DEFAULT_MODEL_COSTS))
(check-true (pair? (assoc "google" PI_DEFAULT_MODEL_COSTS)))
(check-true (pair? (assoc "anthropic" PI_DEFAULT_MODEL_COSTS)))

;; 当前 Claude 模型均有定价
(let ((ids
        (map (lambda (m) (cdr (assoc "id" m)))
          (cdr (assoc "anthropic" PI_DEFAULT_MODEL_COSTS))
        ) ;map
      ) ;ids
     ) ;
  (for-each
    (lambda (id) (check-true (if (member id ids) #t #f)))
    '("claude-opus-5-5" "claude-sonnet-5-5" "claude-haiku-5-5"
      "claude-fable-5-2")
  ) ;for-each
) ;let

(check-report)
