(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-index->cursor
;; 将字符索引转换为字符串游标。
;;
;; 语法
;; ----
;; (string-index->cursor str index)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; index : integer
;; 字符索引位置
;;
;; 返回值
;; ------
;; string-cursor?
;; 索引对应的字符串游标
;;
;; 说明
;; ----
;; 1. string-index->cursor 是 SRFI-130 中的游标转换函数
;; 2. 如果 index 已经是游标，则直接返回
;; 3. 与 (liii string) 的区别：(liii string-cursor) 提供了完整的游标操作支持
;; 4. 性能：O(n)，需要扫描字符串建立位置映射表

;; 基本测试
(let* ((s "abcdef")
       (c0 (string-index->cursor s 0))
       (c3 (string-index->cursor s 3))
       (c6 (string-index->cursor s 6)))
  (check (string-cursor? c0) => #t)
  (check (string-cursor->index s c0) => 0)
  (check (string-cursor->index s c3) => 3)
  (check (string-cursor->index s c6) => 6))

;; 测试中文字符串
(let* ((s "中文测试")
       (c2 (string-index->cursor s 2)))
  (check (string-cursor->index s c2) => 2))

;; 测试emoji字符串
(let* ((s "🎉🎊")
       (c1 (string-index->cursor s 1)))
  (check (string-cursor->index s c1) => 1))

;; 测试越界检查
(check-catch 'value-error
  (string-index->cursor "abc" 4))

(check-catch 'value-error
  (string-index->cursor "abc" -1))


;; 测试传入游标直接返回
(let ((c (string-cursor-start "abc")))
  (check (string-cursor=? (string-index->cursor "abc" c) c) => #t))
;; 测试传入游标直接返回
(let* ((s "abc")
       (c (string-cursor-start s)))
  (check (string-cursor=? c (string-index->cursor s c)) => #t))

(check-report)
