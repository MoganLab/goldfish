(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-object?
;; 判断值是否为 JSON 对象。
;;
;; 语法
;; ----
;; (json-object? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为对象表示形式的 alist 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 在 guenchi json 中，非空 alist 表示对象，空对象使用 `'(())` 表示。
;;
;; 示例
;; ----
;; (json-object? '((name . "Alice"))) => #t
;; (json-object? #(1 2)) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-object? '((name . "Alice"))))
(check-true (json-object? '((a . 1) (b . 2))))
(check-false (json-object? '()))
(check-false (json-object? #(1 2)))
(check-false (json-object? "{}"))
(check-false (json-object? '(1 2 3)))

(check-report)

