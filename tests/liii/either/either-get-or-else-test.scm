(import (liii check)
        (liii error)
        (liii either))

(check-set-mode! 'report-failed)

;; either-get-or-else
;; 获取 Right 中的值，若为 Left 则返回默认值。
;;
;; 语法
;; ----
;; (either-get-or-else either default)
;;
;; 参数
;; ----
;; either : either
;; 输入 Either 值。
;;
;; default : any?
;; 当输入为 Left 时返回的默认值。
;;
;; 返回值
;; ----
;; any?
;; Right 时返回其中的值，Left 时返回 default。
;;
;; 注意
;; ----
;; 不会修改原 Either 值。
;;
;; 示例
;; ----
;; (either-get-or-else (from-right 42) 0) => 42
;; (either-get-or-else (from-left "error") 0) => 0
;;
;; 错误处理
;; ----
;; type-error 当 either 不是 Either 时

(check (either-get-or-else (from-right 42) 0) => 42)
(check (either-get-or-else (from-left "error") 0) => 0)

(check-catch 'type-error (either-get-or-else "not-either" 0))

(check-report)
