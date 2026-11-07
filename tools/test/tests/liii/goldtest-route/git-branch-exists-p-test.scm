;; git-branch-exists?
;; TODO: 补充 git-branch-exists? 的简要描述。
;;
;; 语法
;; ----
;; (git-branch-exists? ...)
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

;; TODO: 补充 git-branch-exists? 的断言用例。
(check-true (procedure? git-branch-exists?))

(check-report)
