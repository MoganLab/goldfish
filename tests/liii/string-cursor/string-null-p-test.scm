(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-null?
;; 判断字符串是否为空。
;;
;; 语法
;; ----
;; (string-null? s)
;;
;; 参数
;; ----
;; s : string
;; 要判断的字符串
;;
;; 返回值
;; ------
;; boolean?
;; 字符串为空返回#t，否则返回#f
;;
;; 说明
;; ----
;; 1. 空字符串返回#t
;; 2. 包含任意字符（包括中文、emoji）的字符串返回#f
;; 3. 性能：O(1)，直接检查长度
;; 4. 支持 Unicode 字符串的空判断
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-null? 函数
;; 参见: gf doc liii/string "string-null?"

;; 基本测试
(check (string-null? "") => #t)
(check (string-null? "a") => #f)
(check (string-null? "中文") => #f)

(check-report)
