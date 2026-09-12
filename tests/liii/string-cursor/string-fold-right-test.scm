(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-fold-right
;; 从右到左遍历字符串，将每个字符与累积值进行折叠运算。
;;
;; 语法
;; ----
;; (string-fold-right kons knil s)
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
;; 1. 遍历方向为从右到左
;; 2. 适用于 ASCII、中文、emoji 等各种 Unicode 字符
;; 3. 空字符串返回 knil
;; 4. 性能：O(n)，n 为字符串字符数
;; 5. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确折叠
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-fold-right 函数
;; 参见: gf doc liii/string "string-fold-right"

;; 基本测试 - ASCII，从右到左折叠为顺序列表
(check (string-fold-right cons '() "abc") => '(#\a #\b #\c))

;; 字符串拼接测试
(check (string-fold-right (lambda (c acc) (string-append (string c) acc)) "" "abc")
  =>
  "abc"
) ;check

;; 中文测试
(check (string-fold-right (lambda (c count) (+ count 1)) 0 "中文") => 2)

;; 测试使用游标作为 start/end
(let* ((s "abc") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (string-fold-right (lambda (c count) (+ count 1)) 0 s start end) => 3)
) ;let*
(check-report)
