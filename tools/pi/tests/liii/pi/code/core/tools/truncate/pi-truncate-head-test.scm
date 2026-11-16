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

(import (liii pi ai tools trunc-result)
  (liii check)
  (liii pi code core session-manager)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

;; pi-truncate-head
;; 保留头部内容截断文本。
(let ((res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check (pi-trunc-result-truncated res) => #f)
  (check (pi-trunc-result-content res) => "line1\nline2\nline3")
  (check (pi-trunc-result-output-lines res) => 3)
  (check (pi-trunc-result-total-lines res) => 3)
) ;let

(let ((res (pi-truncate-head "line1\nline2\nline3\nline4" 2 1000)))
  (check (pi-trunc-result-truncated res) => #t)
  (check (pi-trunc-result-truncated-by res) => 'lines)
  (check (pi-trunc-result-content res) => "line1\nline2")
  (check (pi-trunc-result-output-lines res) => 2)
  (check (pi-trunc-result-total-lines res) => 4)
) ;let

(let ((res (pi-truncate-head "12345\n67890\nabcde" 10 12)))
  (check (pi-trunc-result-truncated res) => #t)
  (check (pi-trunc-result-truncated-by res) => 'bytes)
  (check (pi-trunc-result-content res) => "12345\n67890")
  (check (pi-trunc-result-output-lines res) => 2)
) ;let

(let ((res (pi-truncate-head "1234567890\nline2" 10 5)))
  (check (pi-trunc-result-truncated res) => #t)
  (check (pi-trunc-result-truncated-by res) => 'bytes)
  (check (pi-trunc-result-first-line-exceeds-limit? res) => #t)
  (check (pi-trunc-result-content res) => "")
) ;let

(check-report)
