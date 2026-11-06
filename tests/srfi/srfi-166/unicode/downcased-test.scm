;; downcased
;; 格式化组合子：将输出内容转为小写后再交给下游输出。
;;
;; 语法
;; ----
;; (downcased fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 输出时统一小写化的格式化组合子。
;;
;; 说明
;; ----
;; 通过替换输出端口函数实现转换，作用于经由本组合子
;; 输出的每个字符串；与 upcased 可嵌套组合。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

(check (show #f (downcased "ABC")) => "abc")
(check (show #f (downcased "ABC" "DEF")) => "abcdef")
(check (show #f (downcased)) => "")

;; 与 upcased 嵌套：内层小写、外层大写
(check (show #f (upcased "abc" (downcased "GHI"))) => "ABCGHI")

(check-report)
