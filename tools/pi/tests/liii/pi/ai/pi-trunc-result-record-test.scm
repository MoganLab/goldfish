(import (liii check) (liii pi ai event) (liii pi ai tools record) (liii pi tools))

(check-set-mode! 'report-failed)

;; pi-trunc-result-record-test.scm
;; 验证 pi-trunc-result record 构造器、谓词与访问器，以及对 assoc 的平滑兼容。

(define tr1
  (pi-trunc-result "some content" #t 'lines 100 5000 50 2500 #f #f)
) ;define

(check-true (pi-trunc-result? tr1))
(check (pi-trunc-result-content tr1) => "some content")
(check (pi-trunc-result-truncated tr1) => #t)
(check (pi-trunc-result-truncated? tr1) => #t)
(check (pi-trunc-result-truncated-by tr1) => 'lines)
(check (pi-trunc-result-total-lines tr1) => 100)
(check (pi-trunc-result-total-bytes tr1) => 5000)
(check (pi-trunc-result-output-lines tr1) => 50)
(check (pi-trunc-result-output-bytes tr1) => 2500)
(check (pi-trunc-result-first-line-exceeds-limit? tr1) => #f)
(check (pi-trunc-result-first-line-truncated tr1) => #f)
(check (pi-trunc-result-last-line-partial? tr1) => #f)
(check (pi-trunc-result-last-line-truncated tr1) => #f)

;; assoc 兼容访问
(check (cdr (assoc 'content tr1)) => "some content")
(check (cdr (assoc "content" tr1)) => "some content")
(check (cdr (assoc 'truncated tr1)) => #t)
(check (cdr (assoc "truncated" tr1)) => #t)
(check (cdr (assoc 'truncated-by tr1)) => 'lines)
(check (cdr (assoc "truncated-by" tr1)) => 'lines)
(check (cdr (assoc 'total-lines tr1)) => 100)
(check (cdr (assoc "total-lines" tr1)) => 100)
(check (cdr (assoc 'total-bytes tr1)) => 5000)
(check (cdr (assoc "total-bytes" tr1)) => 5000)
(check (cdr (assoc 'output-lines tr1)) => 50)
(check (cdr (assoc "output-lines" tr1)) => 50)
(check (cdr (assoc 'output-bytes tr1)) => 2500)
(check (cdr (assoc "output-bytes" tr1)) => 2500)
(check (cdr (assoc 'first-line-exceeds-limit? tr1)) => #f)
(check (cdr (assoc 'first-line-truncated tr1)) => #f)
(check (cdr (assoc 'last-line-partial? tr1)) => #f)
(check (cdr (assoc 'last-line-truncated tr1)) => #f)

;; 验证 pi-truncate-head 返回 record
(let ((head-res (pi-truncate-head "line1\nline2\nline3" 10 1000)))
  (check-true (pi-trunc-result? head-res))
  (check (pi-trunc-result-content head-res) => "line1\nline2\nline3")
  (check (pi-trunc-result-truncated head-res) => #f)
  (check (pi-trunc-result-output-lines head-res) => 3)
  (check (pi-trunc-result-total-lines head-res) => 3)
) ;let

;; 验证 pi-truncate-tail 返回 record
(let ((tail-res (pi-truncate-tail "line1\nline2\nline3" 10 1000)))
  (check-true (pi-trunc-result? tail-res))
  (check (pi-trunc-result-content tail-res) => "line1\nline2\nline3")
  (check (pi-trunc-result-truncated tail-res) => #f)
) ;let

(check-report)
