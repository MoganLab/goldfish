(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-ref-boolean
;; 安全读取 JSON 中的布尔值。
;;
;; 语法
;; ----
;; (json-ref-boolean json key default-value)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象。
;;
;; key : symbol? | string? | integer? | boolean?
;; 要读取的键名。
;;
;; default-value : boolean?
;; 当键不存在或值不是布尔时返回的默认值。
;;
;; 返回值
;; ----
;; boolean
;; 若读取到布尔值则返回该值，否则返回默认值。
;;
;; 注意
;; ----
;; 这是类型安全获取器，不会抛类型错误。
;;
;; 示例
;; ----
;; (json-ref-boolean j0 'active #f) => #t
;;
;; 错误处理
;; ----
;; 无。

(let* ((j0 '((active . #t) (verified . #f) (name . "Alice"))
       ) ;j0
      ) ;
  (check (json-ref-boolean j0 'active #f)
    =>
    #t
  ) ;check
  (check (json-ref-boolean j0 'verified #t)
    =>
    #f
  ) ;check
  (check (json-ref-boolean j0 'name #f)
    =>
    #f
  ) ;check
  (check (json-ref-boolean j0 'nonexistent #t)
    =>
    #t
  ) ;check
) ;let*

(check-report)
