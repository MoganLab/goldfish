;; pi-truncate-tail
;; 保留尾部内容截断文本
;;
;; 语法
;; ----
;; (pi-truncate-tail ...)
;;
;; 参数
;; ----
;; 见各断言场景
;;
;; 返回值
;; ------
;; 见各断言场景

(import (liii pi ai tools trunc-result)
  (liii check)
  (liii pi code core session-manager)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

;; pi-truncate-tail
;; 保留尾部内容截断文本。
(let ((res (pi-truncate-tail "line1\nline2" 10 1000)))
  (check (pi-trunc-result-truncated res) => #f)
) ;let

(let* ((text "line1\nline2\nline3\nline4\nline5")
       (res (pi-truncate-tail text 2 1000))
      ) ;
  (check (pi-trunc-result-truncated res) => #t)
  (check (pi-trunc-result-truncated-by res) => 'lines)
  (check (pi-trunc-result-total-lines res) => 5)
  (check (pi-trunc-result-output-lines res) => 2)
  (check (pi-trunc-result-content res) => "line4\nline5")
) ;let*

(let* ((text "12345\n67890\nabcde") (res (pi-truncate-tail text 100 12)))
  (check (pi-trunc-result-truncated res) => #t)
  (check (pi-trunc-result-truncated-by res) => 'bytes)
  (check (pi-trunc-result-content res) => "67890\nabcde")
) ;let*

(check-report)
