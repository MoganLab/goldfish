;; git-current-branch
;; TODO: 补充 git-current-branch 的简要描述。
;;
;; 语法
;; ----
;; (git-current-branch ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-route))

(check-set-mode! 'report-failed)

;; TODO: 补充 git-current-branch 的断言用例。
(check-true (procedure? git-current-branch))

(check-report)
