(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string->list/cursors
;; 将字符串转换为字符列表。
;;
;; 语法
;; ----
;; (string->list/cursors s [start end])
;;
;; 参数
;; ----
;; s : string
;; 源字符串
;;
;; start : integer (可选)
;; 起始位置，默认为0
;;
;; end : integer (可选)
;; 结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; list?
;; 字符列表
;;
;; 说明
;; ----
;; 1. 返回新分配的列表
;; 2. 与 (liii string) 的区别：(liii string-cursor) 按字符转换，支持 Unicode
;; 3. 性能：O(n)，n 为子串字符数

(check (string->list/cursors "abc") => '(#\a #\b #\c))
(check (string->list/cursors "abc" 1 2) => '(#\b))
(check (string->list/cursors "") => '())

;; Unicode 测试
(check (length (string->list/cursors "中文")) => 2)
(check (char->integer (car (string->list/cursors "中文"))) => 20013)

;; 测试使用游标作为 start/end
(let* ((s "abc") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (length (string->list/cursors s start end)) => 3)
) ;let*
(check-report)
