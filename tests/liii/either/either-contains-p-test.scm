(import (liii check)
        (liii error)
        (liii either))

(check-set-mode! 'report-failed)

;; either-contains?
;; 判断 Right 中是否包含指定值。
;;
;; 语法
;; ----
;; (either-contains? either x)
;;
;; 参数
;; ----
;; either : either
;; 输入 Either 值。
;;
;; x : any?
;; 要匹配的值。
;;
;; 返回值
;; ----
;; boolean
;; 仅当输入为 Right 且其值等于 x 时返回#t，否则返回#f。
;;
;; 注意
;; ----
;; Left 总是返回 #f。
;;
;; 示例
;; ----
;; (either-contains? (from-right 10) 10) => #t
;; (either-contains? (from-left 10) 10) => #f
;;
;; 错误处理
;; ----
;; type-error 当 either 不是 Either 时

(check-true (either-contains? (from-right 10) 10))
(check-false (either-contains? (from-right 11) 10))
(check-false (either-contains? (from-left 10) 10))

(check-catch 'type-error (either-contains? "not-either" 1))

(check-report)
