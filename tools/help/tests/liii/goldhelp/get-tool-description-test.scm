;; get-tool-description
;; TODO: 补充 get-tool-description 的简要描述。
;;
;; 语法
;; ----
;; (get-tool-description ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldhelp))

(check-set-mode! 'report-failed)

;; TODO: 补充 get-tool-description 的断言用例。
(check-true (procedure? get-tool-description))

(check-report)
