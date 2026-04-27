(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string->vector/cursors
;; 将字符串转换为字符向量。
;;
;; 语法
;; ----
;; (string->vector/cursors s [start end])
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
;; vector?
;; 字符向量
;;
;; 说明
;; ----
;; 1. 返回新分配的向量
;; 2. 与 (liii string) 的区别：(liii string-cursor) 按字符转换，支持 Unicode
;; 3. 性能：O(n)，n 为子串字符数

(check (string->vector/cursors "abc") => #(#\a #\b #\c))
(check (string->vector/cursors "abc" 1 2) => #(#\b))
(check (string->vector/cursors "") => #())

;; Unicode 测试
(check (vector-length (string->vector/cursors "中文")) => 2)
(check (char->integer (vector-ref (string->vector/cursors "中文") 0)) => 20013)

;; 测试使用游标作为 start/end
(let* ((s "abc")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (vector-length (string->vector/cursors s start end)) => 3))
(check-report)
