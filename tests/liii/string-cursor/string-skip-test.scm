(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-skip
;; 从左向右查找，返回第一个不满足谓词的字符的cursor。
;; 如果没有找到，返回 end cursor。
;;
;; 语法
;; ----
;; (string-skip s pred [start end])
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
;; 1. string-skip 是 string-index 的补集
;; 2. 常用于跳过空白字符等场景
;; 3. 搜索范围是 [start, end)

;; 基本测试
(let ((s "   abc"))
  ;; 跳过空白字符，返回第一个非空白字符 'a' 的cursor
  (check (string-cursor->index s (string-skip s char-whitespace?)) => 3))

;; 测试全部满足谓词的情况
(let ((s "   "))
  (check (string-cursor->index s (string-skip s char-whitespace?)) => 3))

;; 测试全部不满足谓词的情况
(let ((s "abc"))
  (check (string-cursor->index s (string-skip s char-whitespace?)) => 0))

;; 测试空字符串
(let ((s ""))
  (check (string-cursor->index s (string-skip s char-whitespace?)) => 0))

;; 测试带 start/end 参数
(let ((s "abc  def"))
  ;; 从索引3开始，跳过空白，返回 'd' 的索引5
  (check (string-cursor->index s (string-skip s char-whitespace? 3)) => 5))

;; 测试中文
(let ((s "  中文"))
  (check (string-cursor->index s (string-skip s char-whitespace?)) => 2))

(check-report)
