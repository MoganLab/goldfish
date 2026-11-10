;; upcased
;; 格式化组合子：将输出内容转为大写后再交给下游输出。
;;
;; 语法
;; ----
;; (upcased fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 输出时统一大写化的格式化组合子。
;;
;; 说明
;; ----
;; 通过替换输出端口函数实现转换，作用于经由本组合子输出的
;; 每个字符串；与同库 downcased 可嵌套组合。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

(check (show #f (upcased "abc")) => "ABC")
(check (show #f (upcased "abc" "def")) => "ABCDEF")
(check (show #f (upcased)) => "")

;; 多个格式化对象与嵌套组合
(check (show #f (upcased "hello") " " (downcased "WORLD")) => "HELLO world")

(check-report)
