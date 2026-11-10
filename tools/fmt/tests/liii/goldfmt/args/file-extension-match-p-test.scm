;; file-extension-match?
;; TODO: 补充 file-extension-match? 的简要描述。
;;
;; 语法
;; ----
;; (file-extension-match? ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldfmt args))

(check-set-mode! 'report-failed)

;; TODO: 补充 file-extension-match? 的断言用例。
(check-true (procedure? file-extension-match?))

(check-report)
