;; ???
;; 抛出未知或未实现标记错误。
;;
;; 语法
;; ----
;; (??? . args)
;;
;; 参数
;; ----
;; args : any
;; 错误描述信息。

;; 返回值
;; ----
;; 不返回
;; 抛出对应错误，不返回正常值。

(import (liii check)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

(check-catch '??? (???))
(check-catch '??? (??? "todo implementation"))

(check-report)
