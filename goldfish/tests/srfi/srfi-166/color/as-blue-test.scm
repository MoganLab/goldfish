;; as-blue
;; 终端前景色格式化组合子。将输出内容包裹在 ANSI 前景色 34 控制序列中，并在结束时恢复之前的颜色。
;;
;; 语法
;; ----
;; (as-blue fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 前景色 34（蓝色）控制序列的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (as-blue "blue")) => "\x1b;[34mblue\x1b;[39m")
(check (show #f (as-blue "a" "b")) => "\x1b;[34mab\x1b;[39m")
(check (show #f (as-blue)) => "\x1b;[34m\x1b;[39m")

(check-report)
