(import (liii check) (liii pi code session) (liii pi code tools))

(check-set-mode! 'report-failed)

;; pi-truncate-head
;; 保留头部内容截断文本。
(let ((res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f)
  (check (cdr (assoc 'content res)) => "line1\nline2\nline3")
  (check (cdr (assoc 'output-lines res)) => 3)
  (check (cdr (assoc 'total-lines res)) => 3))

(let ((res (pi-truncate-head "line1\nline2\nline3\nline4" 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'content res)) => "line1\nline2")
  (check (cdr (assoc 'output-lines res)) => 2)
  (check (cdr (assoc 'total-lines res)) => 4))

(let ((res (pi-truncate-head "12345\n67890\nabcde" 10 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'content res)) => "12345\n67890")
  (check (cdr (assoc 'output-lines res)) => 2))

(let ((res (pi-truncate-head "1234567890\nline2" 10 5)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'first-line-exceeds-limit? res)) => #t)
  (check (cdr (assoc 'content res)) => ""))

(check-report)
