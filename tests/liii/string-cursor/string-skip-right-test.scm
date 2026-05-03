(import (liii check)
        (scheme char))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-skip-right
;; 从右向左查找，返回第一个不满足谓词的字符的 successor cursor。
;; 如果没有找到（全部满足），返回 start cursor。
;; 如果全部不满足，返回 end cursor。
;;
;; 语法
;; ----
;; (string-skip-right s pred [start end])
;;
;; 参数
;; ----
;; s : string
;; 要搜索的字符串
;;
;; pred : procedure
;; 一元字符谓词函数
;;
;; start : integer (可选)
;; 搜索起始位置（字符索引），默认为0
;;
;; end : integer (可选)
;; 搜索结束位置（字符索引），默认为字符串字符数
;;
;; 返回值
;; ------
;; string-cursor?
;; 找到则返回 successor cursor，否则返回 start cursor
;;
;; 说明
;; ----
;; 1. string-skip-right 是 string-skip 的反向版本
;; 2. 常用于从右侧跳过空白字符等场景
;; 3. 搜索范围是 [start, end)
;; 4. 与 (liii string) 的区别：(liii string-cursor) 按字符操作，支持 Unicode
;; 5. 性能：O(n)，n 为字符串字符数

;; 基本测试 - ASCII
(let ((s "abc123"))
  ;; 从右向左，第一个不是字母的是 '1'（索引3），successor 是索引6
  (check (string-cursor->index s (string-skip-right s char-alphabetic?)) => 6)
) ;let

;; 测试全部满足谓词的情况
(let ((s "abc"))
  (check (string-cursor->index s (string-skip-right s char-alphabetic?)) => 0)
) ;let

;; 测试全部不满足谓词的情况
(let ((s "123"))
  (check (string-cursor->index s (string-skip-right s char-alphabetic?)) => 3)
) ;let

;; 测试带 start/end 参数
(let ((s "abc123"))
  ;; 在 [0, 4) 范围内，从右往左是 c(2) b(1) a(0)，全部满足，返回 start=0
  (check (string-cursor->index s (string-skip-right s char-alphabetic? 0 3)) => 0)
  ;; 在 [3, 6) 范围内，从右往左是 3(5) 2(4) 1(3)，全部满足（都是数字），返回 start=3
  (check (string-cursor->index s (string-skip-right s char-numeric? 3 6)) => 3)
) ;let

;; 测试空字符串
(let ((s ""))
  (check (string-cursor->index s (string-skip-right s char-alphabetic?)) => 0)
) ;let

;; 测试中文
(let ((s "中文  "))
  ;; 从右往左，第一个不是空白的是 '文'（索引1），successor 是索引2
  (check (string-cursor->index s (string-skip-right s char-whitespace?)) => 2)
) ;let


;; 测试使用游标作为 start/end
(let* ((s "abc123")
       (start (string-cursor-start s))
       (end (string-cursor-end s))
       (result (string-skip-right s char-numeric? start end))
      ) ;
  (check (string-cursor->index s result) => 3)
) ;let*
(check-report)
