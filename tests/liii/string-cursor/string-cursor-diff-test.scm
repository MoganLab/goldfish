(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-diff: 计算两个string cursor之间的字符距离
;; 性能：O(1)，直接计算索引差
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

(check-report)
