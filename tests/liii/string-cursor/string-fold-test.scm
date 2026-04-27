(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-fold
;; 从左到右遍历字符串，将每个字符与累积值进行折叠运算。
;;
;; 语法
;; ----
;; (string-fold kons knil s)
;;
;; 参数
;; ----
;; kons : procedure
;; 折叠函数，接收当前字符和累积值，返回新的累积值
;;
;; knil : any
;; 累积初始值
;;
;; s : string
;; 要遍历的字符串
;;
;; 返回值
;; ------
;; any
;; 最终的累积值
;;
;; 说明
;; ----
;; 1. 遍历方向为从左到右
;; 2. 适用于 ASCII、中文、emoji 等各种 Unicode 字符
;; 3. 空字符串返回 knil
;; 4. 性能：O(n)，n 为字符串字符数

;; 基本测试 - ASCII，从左到右折叠为逆序列表
(check (string-fold (lambda (c acc) (cons c acc)) '() "abc") => '(#\c #\b #\a))

;; 计数测试
(check (string-fold (lambda (c count) (+ count 1)) 0 "abc") => 3)

;; 空字符串测试
(check (string-fold (lambda (c count) (+ count 1)) 0 "") => 0)

;; 中文测试
(check (string-fold (lambda (c count) (+ count 1)) 0 "中文") => 2)

;; Emoji 测试
(check (string-fold (lambda (c count) (+ count 1)) 0 "🎉🎊") => 2)



;; 测试使用游标作为 start/end
(let* ((s "abc")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-fold (lambda (c count) (+ count 1)) 0 s start end) => 3))
(check-report)
