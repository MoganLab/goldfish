(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-null?
;; 判断值是否为 JSON null。
;;
;; 语法
;; ----
;; (json-null? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 等于符号 `null` 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; JSON null 在本库中使用符号 `null` 表示。
;;
;; 示例
;; ----
;; (json-null? 'null) => #t
;; (json-null? '((name . "Alice"))) => #f
;;
;; 错误处理
;; ----
;; 无。

(define bob-pp
  '((bob (age . 18) (sex . male) (name . "Bob") (empty . null)))
) ;define

(check-true (json-null? 'null))
(check-false (json-null? '((name . "Alice")))
) ;check-false
(check-true (json-null? (json-ref bob-pp 'bob 'empty)
            ) ;json-null?
) ;check-true

(check-report)
