(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor=?
;; 性能：O(1)，直接比较索引
(let* ((s "abcde") (c0 (string-index->cursor s 0)) (c1 (string-index->cursor s 1)))
  (check (string-cursor=? c0 c0) => #t)
  (check (string-cursor=? c0 c1) => #f)
  (check (string-cursor=? c1 c0) => #f)
) ;let*

(check-report)
