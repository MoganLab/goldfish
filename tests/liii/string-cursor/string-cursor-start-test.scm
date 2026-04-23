(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-start: 返回字符串的起始cursor
;; 测试空字符串
(let ((c (string-cursor-start "")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "" c) => 0))

;; 测试ASCII字符串
(let ((c (string-cursor-start "abc")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "abc" c) => 0))

;; 测试中文字符串
(let ((c (string-cursor-start "中文")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "中文" c) => 0))

;; 测试emoji字符串
(let ((c (string-cursor-start "🎉🎊")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "🎉🎊" c) => 0))

(check-report)
