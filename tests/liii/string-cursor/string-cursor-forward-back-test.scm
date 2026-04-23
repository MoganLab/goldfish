(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-forward: 向前移动nchars
;; 测试ASCII字符串
(let* ((start (string-cursor-start "abcdef"))
       (fwd2 (string-cursor-forward "abcdef" start 2))
       (fwd0 (string-cursor-forward "abcdef" start 0)))
  (check (string-cursor->index "abcdef" fwd2) => 2)
  (check (string-cursor->index "abcdef" fwd0) => 0))

;; 测试中文字符串
(let* ((start (string-cursor-start "我是中国人"))
       (fwd3 (string-cursor-forward "我是中国人" start 3))
       (end (string-cursor-end "我是中国人")))
  (check (string-cursor->index "我是中国人" fwd3) => 3)
  (check (string-cursor=? fwd3 (string-cursor-back "我是中国人" end 2)) => #t))

;; 测试从end向后退
(let* ((end (string-cursor-end "abc"))
       (back1 (string-cursor-back "abc" end 1))
       (back3 (string-cursor-back "abc" end 3)))
  (check (string-cursor->index "abc" back1) => 2)
  (check (string-cursor->index "abc" back3) => 0))

;; 测试边界：forward 0 应该返回原cursor
(let* ((start (string-cursor-start "abc"))
       (fwd0 (string-cursor-forward "abc" start 0)))
  (check (string-cursor=? start fwd0) => #t))

;; 测试越界应该报错
(check-catch 'value-error
  (string-cursor-forward "abc" (string-cursor-start "abc") 4))

(check-catch 'value-error
  (string-cursor-back "abc" (string-cursor-start "abc") 1))

(check-report)
