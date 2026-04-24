(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-concatenate
;; 将字符串列表连接成一个字符串。
;;
;; 语法
;; ----
;; (string-concatenate string-list)
;;
;; 参数
;; ----
;; string-list : list of string?
;; 要连接的字符串列表
;;
;; 返回值
;; ------
;; string?
;; 连接后的新字符串
;;
;; 说明
;; ----
;; 1. 列表为空时返回空字符串
;; 2. 支持 Unicode 字符串
;; 3. 性能：O(n²)，n 为字符串列表长度，因为每次 string-append 都会创建新字符串

(check (string-concatenate '("a" "b" "c")) => "abc")
(check (string-concatenate '()) => "")
(check (string-concatenate '("中文" "测试")) => "中文测试")

(check-report)
