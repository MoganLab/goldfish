(import (liii check)
  (liii error)
  (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-any
;; 对 Either 做存在量词判断。
;;
;; 语法
;; ----
;; (either-any pred either)
;;
;; 参数
;; ----
;; pred : procedure?
;; 要应用到 Right 值上的谓词。
;;
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; boolean
;; Right 时返回 pred 的结果；Left 时返回 #f。
;;
;; 注意
;; ----
;; Left 的返回值体现“无元素可满足条件”。
;;
;; 示例
;; ----
;; (either-any even? (from-right 10)) => #t
;; (either-any even? (from-left "error")) => #f
;;
;; 错误处理
;; ----
;; type-error 当 pred 不是过程或 either 不是 Either 时

(check-true (either-any even? (from-right 10))
) ;check-true
(check-false (either-any even? (from-right 11))
) ;check-false
(check-false (either-any even? (from-left "error"))
) ;check-false

(check-catch 'type-error
  (either-any even? "not-either")
) ;check-catch
(check-catch 'type-error
  (either-any "not-a-proc"
    (from-right 10)
  ) ;either-any
) ;check-catch

(check-report)
