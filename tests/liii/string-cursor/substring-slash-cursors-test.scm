(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; substring/cursors
;; 按cursor截取子串。
;;
;; 语法
;; ----
;; (substring/cursors str start end)
;;
;; 参数
;; ----
;; str : string
;; 源字符串
;;
;; start, end : string-cursor? 或 integer?
;; 起始和结束位置
;;
;; 返回值
;; ------
;; string?
;; 截取后的子串
;;
;; 说明
;; ----
;; 1. substring/cursors 是 SRFI-130 中的字符串截取函数
;; 2. start 和 end 必须同为游标或同为整数索引
;; 3. 与 (liii string) 中的 substring 功能类似，但支持游标参数
;; 4. 性能：O(n)，n 为子串字符数
;; 5. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确截取
;;
;; 相关实现
;; --------
;; (liii string) 库中提供了 substring 函数，使用整数索引
;; 参见: gf doc liii/string "substring"

;; 测试ASCII
(let ((s "abcdef"))
  (check (substring/cursors s (string-cursor-start s) (string-cursor-end s))
    =>
    "abcdef"
  ) ;check
  (check (substring/cursors s (string-index->cursor s 1) (string-index->cursor s 4))
    =>
    "bcd"
  ) ;check
  (check (substring/cursors s (string-index->cursor s 0) (string-index->cursor s 0))
    =>
    ""
  ) ;check
) ;let

;; 测试中文
(let ((s "我是中国人"))
  (check (substring/cursors s (string-cursor-start s) (string-cursor-end s))
    =>
    "我是中国人"
  ) ;check
  (check (substring/cursors s (string-index->cursor s 1) (string-index->cursor s 4))
    =>
    "是中国"
  ) ;check
  (check (substring/cursors s (string-index->cursor s 2) (string-index->cursor s 2))
    =>
    ""
  ) ;check
) ;let

;; 测试emoji
(let ((s "🎉🎊🎁"))
  (check (substring/cursors s (string-cursor-start s) (string-cursor-end s))
    =>
    "🎉🎊🎁"
  ) ;check
  (check (substring/cursors s (string-index->cursor s 1) (string-index->cursor s 3))
    =>
    "🎊🎁"
  ) ;check
) ;let

;; 测试使用整数索引
(check (substring/cursors "abc" 1 3) => "bc")
(check (substring/cursors "中文" 0 1) => "中")


;; 测试使用游标作为 start/end
(let* ((s "abcdef") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (substring/cursors s start end) => "abcdef")
) ;let*

;; 测试混合 index/cursor 参数应该报错
(check-catch 'type-error (substring/cursors "abc" 0 (string-cursor-end "abc")))
(check-report)
