(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-ref/cursor: 按cursor取完整Unicode字符
;; 测试ASCII
(let ((s "abc"))
  (check (string-ref/cursor s (string-cursor-start s)) => #\a)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\b)
  (check (string-ref/cursor s (string-index->cursor s 2)) => #\c))

;; 测试中文
(let ((s "中文"))
  (check (string-ref/cursor s (string-cursor-start s)) => #\中)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\文))

;; 测试emoji
(let ((s "🎉🎊"))
  (check (string-ref/cursor s (string-cursor-start s)) => #\🎉)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\🎊))

;; 测试ASCII+中文混合
(let ((s "a中b文"))
  (check (string-ref/cursor s (string-index->cursor s 0)) => #\a)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\中)
  (check (string-ref/cursor s (string-index->cursor s 2)) => #\b)
  (check (string-ref/cursor s (string-index->cursor s 3)) => #\文))

;; substring/cursors: 按cursor截取子串
;; 测试ASCII
(let ((s "abcdef"))
  (check (substring/cursors s (string-cursor-start s) (string-cursor-end s)) => "abcdef")
  (check (substring/cursors s (string-index->cursor s 1) (string-index->cursor s 4)) => "bcd")
  (check (substring/cursors s (string-index->cursor s 0) (string-index->cursor s 0)) => ""))

;; 测试中文
(let ((s "我是中国人"))
  (check (substring/cursors s (string-cursor-start s) (string-cursor-end s)) => "我是中国人")
  (check (substring/cursors s (string-index->cursor s 1) (string-index->cursor s 4)) => "是中国")
  (check (substring/cursors s (string-index->cursor s 2) (string-index->cursor s 2)) => ""))

;; 测试emoji
(let ((s "🎉🎊🎁"))
  (check (substring/cursors s (string-cursor-start s) (string-cursor-end s)) => "🎉🎊🎁")
  (check (substring/cursors s (string-index->cursor s 1) (string-index->cursor s 3)) => "🎊🎁"))

;; 测试使用index而不是cursor
(check (substring/cursors "abc" 1 3) => "bc")
(check (substring/cursors "中文" 0 1) => "中")

;; string-copy/cursors
(let ((s "abcdef"))
  (check (string-copy/cursors s) => "abcdef")
  (check (string-copy/cursors s 1) => "bcdef")
  (check (string-copy/cursors s 1 4) => "bcd")
  (check (string-copy/cursors s 0 0) => ""))

(let ((s "中文测试"))
  (check (string-copy/cursors s) => "中文测试")
  (check (string-copy/cursors s 1 3) => "文测"))

(check-report)
