(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-diff
;; 计算两个 string cursor 之间的字符距离。
;;
;; 语法
;; ----
;; (string-cursor-diff str start end)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; start, end : integer 或 string-cursor?
;; 起始和结束位置
;;
;; 返回值
;; ------
;; integer
;; 两个游标之间的字符距离
;;
;; 说明
;; ----
;; 1. string-cursor-diff 是 SRFI-130 中的游标操作函数
;; 2. start 和 end 可以是整数索引或游标，但必须同为一种类型
;; 3. 如果 start > end，会报错
;; 4. 性能：O(1)，直接计算索引差
;; 5. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确距离计算
;;
;; 相关实现
;; --------
;; (liii string-cursor) 独有函数，无 (liii string) 对应版本
;; 参见: gf doc liii/string-cursor "string-cursor-diff"
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

;; 测试中文
(let* ((s "中文测试")
        (start (string-cursor-start s))
        (end (string-cursor-end s))
       ) ;
  (check (string-cursor-diff s start end) => 4)
) ;let*

;; 测试 Emoji
(let* ((s "🎉🚀🎁")
        (start (string-cursor-start s))
        (end (string-cursor-end s))
       ) ;
  (check (string-cursor-diff s start end) => 3)
) ;let*

;; 测试 start > end 报错
(check-catch 'value-error (string-cursor-diff "abc" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-cursor-diff "abc" -1 2))
(check-catch 'value-error (string-cursor-diff "abc" 0 -1))

(check-report)
