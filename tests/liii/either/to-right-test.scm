(import (liii check)
  (liii error)
  (liii either)
) ;import

(check-set-mode! 'report-failed)

;; to-right
;; 从 Right 状态的 Either 中提取内部值。
;;
;; 语法
;; ----
;; (to-right either)
;;
;; 参数
;; ----
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; any?
;; 若输入为 Right，则返回其中存放的值。
;;
;; 注意
;; ----
;; 对非 Either 或 Left 调用时会报错。
;;
;; 示例
;; ----
;; (to-right (from-right 100)) => 100
;;
;; 错误处理
;; ----
;; type-error 当输入不是 Either 时
;; value-error 当输入是 Left 时

(check (to-right (from-right "success data"))
  =>
  "success data"
) ;check
(check (to-right (from-right 100))
  =>
  100
) ;check
(check (to-right (from-right '(1 2 3)))
  =>
  '(1 2 3)
) ;check

(check-catch 'type-error (to-right '()))
(check-catch 'value-error
  (to-right (from-left "I am Left"))
) ;check-catch

(check-report)
