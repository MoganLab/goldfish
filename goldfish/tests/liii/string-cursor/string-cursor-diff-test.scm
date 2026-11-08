;; string-cursor-diff
;; 计算两个字符串游标之间的字符距离。
;;
;; 语法
;; ----
;; (string-cursor-diff str start end)
;;
;; 参数
;; ----
;; str : string?
;; 游标所属的源字符串。
;;
;; start : string-cursor? 或 integer?
;; 起始游标，也可以直接给出整数索引。
;;
;; end : string-cursor? 或 integer?
;; 结束游标，也可以直接给出整数索引。
;;
;; 返回值
;; ----
;; integer
;; 返回两个游标之间的字符距离（end 位置减去 start 位置）。
;;
;; 注意
;; ----
;; 性能：O(1)，直接计算索引差。
;;
;; 错误处理
;; ----
;; type-error 当参数既不是游标也不是整数索引时抛出；
;; value-error 当索引为负数或 start 晚于 end 时抛出；
;; out-of-range 当游标越界时抛出。

(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

(let* ((s "abcdef")
       (start (string-cursor-start s))
       (end (string-cursor-end s))
       (mid (string-index->cursor s 3))
      ) ;
  (check (string-cursor-diff s start end) => 6)
  (check (string-cursor-diff s start mid) => 3)
  (check (string-cursor-diff s mid end) => 3)
  (check (string-cursor-diff s start start) => 0)
) ;let*

;; 测试使用整数索引
(check (string-cursor-diff "abc" 0 3) => 3)
(check (string-cursor-diff "abc" 1 2) => 1)

;; 测试混合类型报错
(check-catch 'type-error (string-cursor-diff "abc" 0 (string-cursor-end "abc")))

;; 测试 start > end 报错
(check-catch 'value-error (string-cursor-diff "abc" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-cursor-diff "abc" -1 2))
(check-catch 'value-error (string-cursor-diff "abc" 0 -1))

;; 测试游标越界报错
(check-catch 'out-of-range (string-cursor-diff "a" -2 -2000000002))

(check-report)
