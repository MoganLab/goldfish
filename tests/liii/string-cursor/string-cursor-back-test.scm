(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-back
;; 向后移动nchars个位置。
;;
;; 语法
;; ----
;; (string-cursor-back str cursor nchars)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; cursor : string-cursor? 或 integer?
;; 当前游标位置
;;
;; nchars : integer
;; 要移动的字符数
;;
;; 返回值
;; ------
;; string-cursor?
;; 移动后的游标位置
;;
;; 说明
;; ----
;; 1. string-cursor-back 是 SRFI-130 中的游标导航函数
;; 2. 如果 cursor 是整数，会先转换为游标
;; 3. 如果移动超出范围，会报错
;; 4. 性能：O(nchars)

;; 测试ASCII字符串
(let* ((end (string-cursor-end "abcdef"))
       (back2 (string-cursor-back "abcdef" end 2))
       (back0 (string-cursor-back "abcdef" end 0)))
  (check (string-cursor->index "abcdef" back2) => 4)
  (check (string-cursor->index "abcdef" back0) => 6))

;; 测试中文字符串
(let* ((end (string-cursor-end "我是中国人"))
       (back3 (string-cursor-back "我是中国人" end 3)))
  (check (string-cursor->index "我是中国人" back3) => 2))

;; 测试从start向前进（结合forward）
(let* ((start (string-cursor-start "abc"))
       (fwd1 (string-cursor-forward "abc" start 1))
       (fwd3 (string-cursor-forward "abc" start 3)))
  (check (string-cursor->index "abc" fwd1) => 1)
  (check (string-cursor->index "abc" fwd3) => 3))

;; 测试边界：back 0 应该返回原cursor
(let* ((end (string-cursor-end "abc"))
       (back0 (string-cursor-back "abc" end 0)))
  (check (string-cursor=? end back0) => #t))

;; 测试越界应该报错
(check-catch 'value-error
  (string-cursor-back "abc" (string-cursor-start "abc") 1))

;; 测试使用整数索引
(let* ((s "abc")
       (back (string-cursor-back s 2 1)))
  (check (string-cursor->index s back) => 1))

(check-report)
