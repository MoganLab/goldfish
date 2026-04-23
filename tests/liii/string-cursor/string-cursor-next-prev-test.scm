(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-next: 返回下一个cursor
;; 测试ASCII字符串
(let* ((start (string-cursor-start "abc"))
       (next1 (string-cursor-next "abc" start))
       (next2 (string-cursor-next "abc" next1))
       (end (string-cursor-end "abc")))
  (check (string-cursor->index "abc" next1) => 1)
  (check (string-cursor->index "abc" next2) => 2)
  (check (string-cursor=? next2 (string-cursor-prev "abc" end)) => #t))

;; 测试中文字符串
(let* ((start (string-cursor-start "中文"))
       (next1 (string-cursor-next "中文" start))
       (end (string-cursor-end "中文")))
  (check (string-cursor->index "中文" next1) => 1)
  (check (string-cursor=? next1 (string-cursor-prev "中文" end)) => #t))

;; 测试emoji字符串
(let* ((start (string-cursor-start "🎉🎊"))
       (next1 (string-cursor-next "🎉🎊" start))
       (end (string-cursor-end "🎉🎊")))
  (check (string-cursor->index "🎉🎊" next1) => 1)
  (check (string-cursor=? next1 (string-cursor-prev "🎉🎊" end)) => #t))

;; 测试空字符串不能有next
(check-catch 'value-error
  (string-cursor-next "" (string-cursor-start "")))

;; 测试到达end后不能再next
(check-catch 'value-error
  (let ((end (string-cursor-end "a")))
    (string-cursor-next "a" end)))

;; 测试到达start后不能再prev
(check-catch 'value-error
  (let ((start (string-cursor-start "a")))
    (string-cursor-prev "a" start)))

(check-report)
