;; %dump-captured-tail
;; TODO: 补充 %dump-captured-tail 的简要描述。
;;
;; 语法
;; ----
;; (%dump-captured-tail ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; TODO: 补充 %dump-captured-tail 的断言用例。
(check-true (procedure? %dump-captured-tail))

(check-report)
