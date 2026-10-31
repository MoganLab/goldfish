(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; absent
;; 缺失值的唯一标记。
;;
;; 语法
;; ----
;; absent
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list
;; 值为 (absent) 的列表，用作“键不存在”的哨兵。
;;
;; 注意
;; ----
;; 该标记与普通列表不同，可用于区分“键不存在”与“键存在但值为空列表”。
;;
;; 示例
;; ----
;; (car absent) => 'absent
;;
;; 错误处理
;; ----
;; 无。

(check-true (pair? absent))
(check (car absent) => 'absent)
(check (length absent) => 1)
(check-false (equal? absent '()))
(check-false (null? absent))

(check-report)
