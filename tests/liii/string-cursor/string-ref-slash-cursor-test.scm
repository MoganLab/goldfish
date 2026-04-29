(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-ref/cursor
;; 按cursor取完整Unicode字符。
;;
;; 语法
;; ----
;; (string-ref/cursor str cursor)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; cursor : string-cursor? 或 integer?
;; 字符串游标或字符索引
;;
;; 返回值
;; ------
;; char?
;; 游标位置的完整Unicode字符
;;
;; 说明
;; ----
;; 1. string-ref/cursor 是 SRFI-130 中的字符串访问函数
;; 2. 支持使用游标或整数索引
;; 3. 与 (scheme base) 中的 string-ref 不同，本函数按字符访问而非按字节
;; 4. 与 (liii string) 中的 string-ref 功能类似，但支持游标参数
;; 5. 性能：O(1)，当传入游标时；O(n) 当传入整数索引时

;; 测试ASCII
(let ((s "abc"))
  (check (string-ref/cursor s (string-cursor-start s)) => #\a)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\b)
  (check (string-ref/cursor s (string-index->cursor s 2)) => #\c)
) ;let

;; 测试中文
(let ((s "中文"))
  (check (string-ref/cursor s (string-cursor-start s)) => #\中)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\文)
) ;let

;; 测试emoji
(let ((s "🎉🎊"))
  (check (string-ref/cursor s (string-cursor-start s)) => #\🎉)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\🎊)
) ;let

;; 测试ASCII+中文混合
(let ((s "a中b文"))
  (check (string-ref/cursor s (string-index->cursor s 0)) => #\a)
  (check (string-ref/cursor s (string-index->cursor s 1)) => #\中)
  (check (string-ref/cursor s (string-index->cursor s 2)) => #\b)
  (check (string-ref/cursor s (string-index->cursor s 3)) => #\文)
) ;let

;; 测试使用整数索引
(check (string-ref/cursor "abc" 0) => #\a)
(check (string-ref/cursor "abc" 1) => #\b)
(check (string-ref/cursor "中文" 0) => #\中)

;; 测试使用整数索引
(check (string-ref/cursor "abc" 0) => #\a)
(check (string-ref/cursor "abc" 2) => #\c)

(check-report)
