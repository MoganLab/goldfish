(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-join
;; 使用分隔符连接字符串列表。
;;
;; 语法
;; ----
;; (string-join string-list [delimiter grammar])
;;
;; 参数
;; ----
;; string-list : list of string?
;; 要连接的字符串列表
;;
;; delimiter : string (可选)
;; 分隔符，默认为 " "
;;
;; grammar : symbol (可选)
;; 连接语法，可选值：infix、strict-infix、suffix、prefix，默认为 infix
;;
;; 返回值
;; ------
;; string?
;; 连接后的字符串
;;
;; 说明
;; ----
;; 1. infix：在元素之间插入分隔符
;; 2. strict-infix：要求列表非空，否则报错
;; 3. suffix：在元素之间和末尾插入分隔符
;; 4. prefix：在元素之间和开头插入分隔符
;; 5. 与 (liii string) 的区别：(liii string-cursor) 支持 Unicode 字符串
;; 6. 性能：O(n)，n 为所有字符串总长度

(check (string-join '("a" "b" "c")) => "a b c")
(check (string-join '("a" "b" "c") ":") => "a:b:c")
(check (string-join '()) => "")
(check (string-join '("a" "b" "c") ":" 'suffix) => "a:b:c:")
(check (string-join '("a" "b" "c") ":" 'prefix) => ":a:b:c")

;; Unicode 测试
(check (string-join '("中" "文") "-") => "中-文")

(check-report)
