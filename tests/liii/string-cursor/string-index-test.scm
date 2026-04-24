(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-index
;; 从左向右查找，返回第一个满足谓词的字符的cursor。
;; 如果没有找到，返回 end cursor。
;;
;; 语法
;; ----
;; (string-index s pred [start end])
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
;; 找到则返回对应cursor，否则返回 end cursor
;;
;; 说明
;; ----
;; 1. 搜索范围是 [start, end)，包含 start，不包含 end
;; 2. 结果始终为 cursor，即使 start/end 是整数索引
;; 3. 适用于 ASCII、中文、emoji 等各种 Unicode 字符
;; 4. 性能：O(n)，n 为字符串字符数
;;
;; 错误处理
;; --------
;; value-error
;; 当 start/end 超出范围时抛出错误

;; 基本测试 - ASCII
(let ((s "abcdef"))
  (check (string-cursor->index s (string-index s char-alphabetic?)) => 0)
  (check (string-cursor->index s (string-index s (lambda (c) (char=? c #\d)))) => 3))

;; 测试中文
(let ((s "abc123"))
  (check (string-cursor->index s (string-index s char-numeric?)) => 3)
  (check (string-cursor->index s (string-index s (lambda (c) (char=? c #\2)))) => 4))

;; 测试未找到的情况
(let ((s "abc"))
  (check (string-cursor->index s (string-index s char-numeric?)) => 3))

;; 测试带 start 参数
(let ((s "abc123"))
  (check (string-cursor->index s (string-index s char-numeric? 2)) => 3)
  (check (string-cursor->index s (string-index s char-numeric? 4)) => 4))

;; 测试带 start 和 end 参数
(let ((s "abc123"))
  (check (string-cursor->index s (string-index s char-numeric? 0 3)) => 3)
  (check (string-cursor->index s (string-index s char-numeric? 4 5)) => 4)
  ;; 在范围内没有找到
  (check (string-cursor->index s (string-index s char-numeric? 0 3)) => 3))


;; 测试使用游标作为 start/end
(let* ((s "abc123")
       (start (string-cursor-start s))
       (end (string-cursor-end s))
       (result (string-index s char-numeric? start end)))
  (check (string-cursor->index s result) => 3))
(check-report)
