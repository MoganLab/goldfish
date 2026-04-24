(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-replicate
;; 复制字符串的指定子串，并可以循环填充到指定长度。
;;
;; 语法
;; ----
;; (string-replicate s from to [start end])
;;
;; 参数
;; ----
;; s : string
;; 源字符串
;;
;; from : integer
;; 复制起始位置（在循环索引空间中）
;;
;; to : integer
;; 复制结束位置（在循环索引空间中）
;;
;; start : integer (可选)
;; 源字符串子串起始位置，默认为0
;;
;; end : integer (可选)
;; 源字符串子串结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; string?
;; 复制后的新字符串
;;
;; 说明
;; ----
;; 1. 在循环索引空间 [from, to) 中复制字符，超出子串范围时循环回到子串开头
;; 2. 支持 Unicode 字符串
;; 3. 与 (liii string) 的区别：(liii string-cursor) 按字符操作，(liii string) 按字节操作
;; 4. 性能：O(n)，n 为输出字符串长度

;; 基本测试
(check (string-replicate "abcdef" 2 8) => "cdefab")
(check (string-replicate "abcdef" -2 4) => "efabcd")
(check (string-replicate "abc" 0 7) => "abcabca")
(check (string-replicate "abc" 0 0) => "")

;; Unicode 测试
(check (string-replicate "中文" 0 4) => "中文中文")


;; 测试使用游标作为 start/end
(let* ((s "abc")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-replicate s 0 6 start end) => "abcabc"))
(check-report)
