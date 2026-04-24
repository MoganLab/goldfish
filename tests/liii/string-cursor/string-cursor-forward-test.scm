(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-forward
;; 向前移动nchars个位置。
;;
;; 语法
;; ----
;; (string-cursor-forward str cursor nchars)
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
;; 1. string-cursor-forward 是 SRFI-130 中的游标导航函数
;; 2. 如果 cursor 是整数，会先转换为游标
;; 3. 如果移动超出范围，会报错
;; 4. 性能：O(nchars)

;; 测试ASCII字符串
(let* ((start (string-cursor-start "abcdef"))
       (fwd2 (string-cursor-forward "abcdef" start 2))
       (fwd0 (string-cursor-forward "abcdef" start 0)))
  (check (string-cursor->index "abcdef" fwd2) => 2)
  (check (string-cursor->index "abcdef" fwd0) => 0))

;; 测试中文字符串
(let* ((start (string-cursor-start "我是中国人"))
       (fwd3 (string-cursor-forward "我是中国人" start 3)))
  (check (string-cursor->index "我是中国人" fwd3) => 3))

;; 测试从end向后退（结合back）
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

;; 测试使用整数索引
(let* ((s "abc")
       (fwd (string-cursor-forward s 0 2)))
  (check (string-cursor->index s fwd) => 2))

(check-report)
