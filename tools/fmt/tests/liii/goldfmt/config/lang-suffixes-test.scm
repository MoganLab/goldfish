;; lang-suffixes
;; TODO: 补充 lang-suffixes 的简要描述。
;;
;; 语法
;; ----
;; (lang-suffixes ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt config))

(check-set-mode! 'report-failed)

;; TODO: 补充 lang-suffixes 的断言用例。
(check-true (procedure? lang-suffixes))

(check-report)
