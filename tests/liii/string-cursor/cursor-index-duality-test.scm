(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; 测试 cursor 和 index 双态支持
;; SRFI-130 要求 cursor 操作函数同时接受 cursor 和 integer index

;; string-cursor-next 接受 integer
(let ((s "abc"))
  (check (string-cursor->index s (string-cursor-next s 0)) => 1)
  (check (string-cursor->index s (string-cursor-next s 1)) => 2))

;; string-cursor-prev 接受 integer
(let ((s "abc"))
  (check (string-cursor->index s (string-cursor-prev s 2)) => 1)
  (check (string-cursor->index s (string-cursor-prev s 1)) => 0))

;; string-cursor-forward 接受 integer
(let ((s "abc"))
  (check (string-cursor->index s (string-cursor-forward s 0 2)) => 2)
  (check (string-cursor->index s (string-cursor-forward s 1 1)) => 2))

;; string-cursor-back 接受 integer
(let ((s "abc"))
  (check (string-cursor->index s (string-cursor-back s 2 1)) => 1)
  (check (string-cursor->index s (string-cursor-back s 2 2)) => 0))

;; string-cursor=? 接受 integer
(let ((s "abc"))
  (check (string-cursor=? 0 0) => #t)
  (check (string-cursor=? 0 1) => #f)
  (check (string-cursor=? (string-cursor-start s) 0) => #t))

;; string-cursor<? 接受 integer
(check (string-cursor<? 0 1) => #t)
(check (string-cursor<? 1 0) => #f)

;; string-cursor->index 接受 integer（直接返回）
(check (string-cursor->index "abc" 2) => 2)

;; string-index->cursor 接受 cursor（直接返回）
(let ((c (string-cursor-start "abc")))
  (check (string-cursor=? (string-index->cursor "abc" c) c) => #t))

;; string-ref/cursor 接受 integer
(check (string-ref/cursor "abc" 0) => #\a)
(check (string-ref/cursor "abc" 2) => #\c)

;; string-cursor-diff 接受 integer
(check (string-cursor-diff "abc" 0 3) => 3)
(check (string-cursor-diff "abc" 1 2) => 1)

;; 使用 cursor 作为 start/end 参数
(let ((s "abcdef"))
  (let ((start (string-cursor-start s))
        (end (string-cursor-end s)))
    (check (string-every char-alphabetic? s start end) => #t)
    (check (substring/cursors s start end) => "abcdef")
    (check (string-take s 3) => "abc")))

(check-report)
