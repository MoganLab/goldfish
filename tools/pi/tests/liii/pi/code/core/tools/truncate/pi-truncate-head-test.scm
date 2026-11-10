;; pi-truncate-head
;; 保留头部内容截断文本
;;
;; 语法
;; ----
;; (pi-truncate-head s max-lines max-bytes)
;;
;; 参数
;; ----
;; s : string?；max-lines, max-bytes : integer?
;;
;; 返回值
;; ------
;; alist?，含 content/truncated 等键

(import (liii check)
  (liii pi code core session-manager)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

;; pi-truncate-head
;; 保留头部内容截断文本。
(let ((res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check (cdr (assoc 'truncated res)) => #f)
  (check (cdr (assoc 'content res)) => "line1\nline2\nline3")
  (check (cdr (assoc 'output-lines res)) => 3)
  (check (cdr (assoc 'total-lines res)) => 3)
) ;let

(let ((res (pi-truncate-head "line1\nline2\nline3\nline4" 2 1000)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'lines)
  (check (cdr (assoc 'content res)) => "line1\nline2")
  (check (cdr (assoc 'output-lines res)) => 2)
  (check (cdr (assoc 'total-lines res)) => 4)
) ;let

(let ((res (pi-truncate-head "12345\n67890\nabcde" 10 12)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'content res)) => "12345\n67890")
  (check (cdr (assoc 'output-lines res)) => 2)
) ;let

(let ((res (pi-truncate-head "1234567890\nline2" 10 5)))
  (check (cdr (assoc 'truncated res)) => #t)
  (check (cdr (assoc 'truncated-by res)) => 'bytes)
  (check (cdr (assoc 'first-line-exceeds-limit? res)) => #t)
  (check (cdr (assoc 'content res)) => "")
) ;let

(check-report)
