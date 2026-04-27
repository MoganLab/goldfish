(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-split
;; 使用分隔符将字符串分割成子串列表。
;;
;; 语法
;; ----
;; (string-split s delimiter [grammar limit start end])
;;
;; 参数
;; ----
;; s : string
;; 要分割的字符串
;;
;; delimiter : string
;; 分隔符字符串
;;
;; grammar : symbol (可选)
;; 分割语法，可选值：infix、strict-infix、prefix、suffix，默认为 infix
;;
;; limit : integer (可选)
;; 最大分割次数
;;
;; start : integer (可选)
;; 搜索起始位置，默认为0
;;
;; end : integer (可选)
;; 搜索结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; list of string?
;; 分割后的子串列表
;;
;; 说明
;; ----
;; 1. 支持空分隔符，此时按字符分割
;; 2. 与 (liii string) 的区别：(liii string-cursor) 按字符操作，正确处理 Unicode
;; 3. 性能：O(n×m)，n 为 s 长度，m 为 delimiter 长度

(check (string-split "a:b:c" ":") => '("a" "b" "c"))
(check (string-split "abc" "") => '("a" "b" "c"))
(check (string-split "" ":") => '())
(check (string-split "a::b:c" ":") => '("a" "" "b" "c"))

;; Unicode 测试
(check (string-split "中:文:测试" ":") => '("中" "文" "测试"))


;; 测试使用游标作为 start/end
(let* ((s "a,b,c")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-split s "," 'infix #f start end) => '("a" "b" "c")))
(check-report)
