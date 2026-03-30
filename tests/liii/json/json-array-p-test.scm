(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-array?
;; 判断值是否为 JSON 数组。
;;
;; 语法
;; ----
;; (json-array? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为向量时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 本库使用向量表示 JSON 数组。
;;
;; 示例
;; ----
;; (json-array? #(1 2 3)) => #t
;; (json-array? '(1 2 3)) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-array? #(1 2 3)))
(check-true (json-array? #()))
(check-true (json-array? #("a" "b")))
(check-false (json-array? '(1 2 3)))
(check-false (json-array? "[]"))

(check-report)

