;; handle-repo-target
;; TODO: 补充 handle-repo-target 的简要描述。
;;
;; 语法
;; ----
;; (handle-repo-target ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt repo))

(check-set-mode! 'report-failed)

;; TODO: 补充 handle-repo-target 的断言用例。
(check-true (procedure? handle-repo-target))

(check-report)
