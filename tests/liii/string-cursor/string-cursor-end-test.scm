(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-end: 返回字符串的post-end cursor
;; 测试空字符串
(let ((c (string-cursor-end "")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "" c) => 0))

;; 测试ASCII字符串（3个字符）
(let ((c (string-cursor-end "abc")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "abc" c) => 3))

;; 测试中文字符串（2个字符，但6个字节）
(let ((c (string-cursor-end "中文")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "中文" c) => 2))

;; 测试emoji字符串（2个字符，但8个字节）
(let ((c (string-cursor-end "🎉🎊")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "🎉🎊" c) => 2))

;; 测试ASCII+中文混合
(let ((c (string-cursor-end "a中b文")))
  (check (string-cursor->index "a中b文" c) => 4))

(check-report)
