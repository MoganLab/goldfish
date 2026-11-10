;; lang-extensions
;; TODO: 补充 lang-extensions 的简要描述。
;;
;; 语法
;; ----
;; (lang-extensions ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt lang))

(check-set-mode! 'report-failed)

;; TODO: 补充 lang-extensions 的断言用例。
(check-true (procedure? lang-extensions))

(check-report)
