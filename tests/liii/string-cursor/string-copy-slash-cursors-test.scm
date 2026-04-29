(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-copy/cursors
;; 复制字符串的子串。
;;
;; 语法
;; ----
;; (string-copy/cursors s [start end])
;;
;; 参数
;; ----
;; s : string
;; 源字符串
;;
;; start : integer 或 string-cursor? (可选)
;; 起始位置，默认为0
;;
;; end : integer 或 string-cursor? (可选)
;; 结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; string?
;; 复制后的新字符串
;;
;; 说明
;; ----
;; 1. string-copy/cursors 是 SRFI-130 中的字符串复制函数
;; 2. 支持使用游标或整数索引指定范围
;; 3. start 和 end 必须同为整数或同为游标
;; 4. 与 (liii string) 中的 string-copy 功能类似，但支持游标参数
;; 5. 性能：O(n)，n 为子串字符数

;; 基本测试
(let ((s "abcdef"))
  (check (string-copy/cursors s) => "abcdef")
  (check (string-copy/cursors s 0) => "abcdef")
  (check (string-copy/cursors s 1) => "bcdef")
  (check (string-copy/cursors s 1 4) => "bcd")
  (check (string-copy/cursors s 0 0) => "")
  (check (string-copy/cursors s 3 3) => "")
) ;let

;; 测试中文
(let ((s "中文测试"))
  (check (string-copy/cursors s) => "中文测试")
  (check (string-copy/cursors s 1 3) => "文测")
  (check (string-copy/cursors s 0 1) => "中")
) ;let

;; 测试emoji
(let ((s "🎉🎊🎁"))
  (check (string-copy/cursors s 1 3) => "🎊🎁")
) ;let

;; 测试使用游标参数
(let* ((s "abcdef")
       (start-c (string-cursor-start s))
       (end-c (string-cursor-end s))
       (mid-c (string-index->cursor s 3))
      ) ;
  (check (string-copy/cursors s start-c end-c) => "abcdef")
  (check (string-copy/cursors s start-c mid-c) => "abc")
  (check (string-copy/cursors s mid-c end-c) => "def")
) ;let*

(check-report)
