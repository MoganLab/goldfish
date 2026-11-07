;; pi-truncate-for-summary
;; 按长度上限截断用于摘要的文本。
;;
;; 语法
;; ----
;; (pi-truncate-for-summary text max-chars)
;;
;; 参数
;; ----
;; text : string
;; 待截断文本。
;;
;; max-chars : integer
;; 最大字符数。
;;
;; 返回值
;; ----
;; string
;; 截断后的文本。

(import (liii check) (liii string) (liii pi code core compaction serialize))

(check-set-mode! 'report-failed)

(let ((long (make-string 30 #\x)))
  (check (pi-truncate-for-summary "short" 10) => "short")
  (check (pi-truncate-for-summary long 10)
    =>
    (string-append (make-string 10 #\x) "\n\n[... 20 more characters truncated]")
  ) ;check
) ;let

(check-report)
