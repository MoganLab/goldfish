(import (liii check) (scheme char))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-count
;; 统计字符串中满足给定谓词条件的字符数量。
;;
;; 语法
;; ----
;; (string-count pred s)
;;
;; 参数
;; ----
;; pred : procedure
;; 一元字符谓词函数，返回真表示计数
;;
;; s : string
;; 要统计的字符串
;;
;; 返回值
;; ------
;; integer
;; 满足条件的字符数量
;;
;; 说明
;; ----
;; 1. 适用于 ASCII、中文、emoji 等各种 Unicode 字符
;; 2. 空字符串返回 0
;; 3. 性能：O(n)，n 为检查的字符数

;; 基本测试 - 统计字母字符
(check (string-count char-alphabetic? "abc123") => 3)

;; 统计数字字符
(check (string-count char-numeric? "abc123") => 3)

;; 中文测试
(check (string-count char-alphabetic? "中文") => 2)

;; 空字符串测试
(check (string-count (lambda (c) #t) "") => 0)


;; 测试使用游标作为 start/end
(let* ((s "abc123") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (string-count char-alphabetic? s start end) => 3)
) ;let*
(check-report)
