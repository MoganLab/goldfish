(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor comparison functions
(let* ((s "abcde")
       (c0 (string-index->cursor s 0))
       (c1 (string-index->cursor s 1))
       (c2 (string-index->cursor s 2))
       (c3 (string-index->cursor s 3)))

  ;; =?
  (check (string-cursor=? c0 c0) => #t)
  (check (string-cursor=? c0 c1) => #f)
  (check (string-cursor=? c1 c0) => #f)

  ;; <?
  (check (string-cursor<? c0 c1) => #t)
  (check (string-cursor<? c1 c0) => #f)
  (check (string-cursor<? c2 c2) => #f)
  (check (string-cursor<? c0 c3) => #t)

  ;; >?
  (check (string-cursor>? c1 c0) => #t)
  (check (string-cursor>? c0 c1) => #f)
  (check (string-cursor>? c2 c2) => #f)

  ;; <=?
  (check (string-cursor<=? c0 c1) => #t)
  (check (string-cursor<=? c1 c1) => #t)
  (check (string-cursor<=? c1 c0) => #f)

  ;; >=?
  (check (string-cursor>=? c1 c0) => #t)
  (check (string-cursor>=? c1 c1) => #t)
  (check (string-cursor>=? c0 c1) => #f))

;; diff
(let* ((s "abcdef")
       (start (string-cursor-start s))
       (end (string-cursor-end s))
       (mid (string-index->cursor s 3)))
  (check (string-cursor-diff s start end) => 6)
  (check (string-cursor-diff s start mid) => 3)
  (check (string-cursor-diff s mid end) => 3)
  (check (string-cursor-diff s start start) => 0))

;; cursor->index and index->cursor round-trip
(let* ((s "中文测试")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-cursor->index s start) => 0)
  (check (string-cursor->index s end) => 4)
  (check (string-cursor=? start (string-index->cursor s 0)) => #t)
  (check (string-cursor=? end (string-index->cursor s 4)) => #t))

;; index->cursor 越界检查
(check-catch 'value-error
  (string-index->cursor "abc" 4))

(check-catch 'value-error
  (string-index->cursor "abc" -1))

(check-report)
