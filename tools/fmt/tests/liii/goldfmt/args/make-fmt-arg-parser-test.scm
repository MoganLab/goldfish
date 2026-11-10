;; make-fmt-arg-parser
;; TODO: 补充 make-fmt-arg-parser 的简要描述。
;;
;; 语法
;; ----
;; (make-fmt-arg-parser ...)
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

;; TODO: 补充 make-fmt-arg-parser 的断言用例。
(check-true (procedure? make-fmt-arg-parser))

(check-report)
