;; pi-format-timeout-reason
;; 规范化超时异常消息，保证包含 timed out 字样以便上层重试归类。
;;
;; 语法
;; ----
;; (pi-format-timeout-reason raw-msg timeout-sec)
;;
;; 参数
;; ----
;; raw-msg : string?
;; 原始异常描述。
;; timeout-sec : number? | #f
;; 超时秒数。
;;
;; 返回值
;; ------
;; string?
;; 规范化后的超时错误文本。

(import (liii check) (liii string) (liii pi ai time))

(check-set-mode! 'report-failed)

(check-true (string-contains? (pi-format-timeout-reason "Operation timed out" 5)
              "timed out"
            ) ;string-contains?
) ;check-true
(check-true (string-contains? (pi-format-timeout-reason "Operation too slow" 5) "timed out")
) ;check-true
(check-true (string-contains? (pi-format-timeout-reason "" #f) "timed out"))

(check-report)
