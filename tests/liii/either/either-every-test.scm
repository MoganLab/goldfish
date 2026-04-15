(import (liii check)
  (liii error)
  (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-every
;; 对 Either 做全称量词判断。
;;
;; 语法
;; ----
;; (either-every pred either)
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
;; Right 时返回 pred 的结果；Left 时返回 #t。
;;
;; 注意
;; ----
;; Left 的返回值体现空真性。
;;
;; 示例
;; ----
;; (either-every even? (from-right 10)) => #t
;; (either-every even? (from-left "error")) => #t
;;
;; 错误处理
;; ----
;; type-error 当 pred 不是过程或 either 不是 Either 时

(check-true (either-every even? (from-right 10))
) ;check-true
(check-false (either-every even? (from-right 11))
) ;check-false
(check-true (either-every even? (from-left "error"))
) ;check-true

(check-catch 'type-error
  (either-every even? "not-either")
) ;check-catch
(check-catch 'type-error
  (either-every "not-a-proc"
    (from-right 10)
  ) ;either-every
) ;check-catch

(check-report)
