(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-truncate-tail
;; 保留尾部内容截断文本。
(let ((res (pi-truncate-tail "line1\nline2" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f))

(let* ((text "line1\nline2\nline3\nline4\nline5")
       (res (pi-truncate-tail text 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'total-lines res)) => 5)
  (check (cdr (assoc 'output-lines res)) => 2)
  (check (cdr (assoc 'content res)) => "line4\nline5"))

(let* ((text "12345\n67890\nabcde")
       (res (pi-truncate-tail text 100 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'content res)) => "67890\nabcde"))

(check-report)
