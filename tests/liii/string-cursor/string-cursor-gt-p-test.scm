(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor>?
;; 性能：O(1)，直接比较索引
(let* ((s "abcde")
       (c0 (string-index->cursor s 0))
       (c1 (string-index->cursor s 1))
       (c2 (string-index->cursor s 2)))
  (check (string-cursor>? c1 c0) => #t)
  (check (string-cursor>? c0 c1) => #f)
  (check (string-cursor>? c2 c2) => #f))

(check-report)
