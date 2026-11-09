;; as-underline
;; 终端文本修饰组合子。为输出内容添加下划线样式，并在结束时恢复。
;;
;; 语法
;; ----
;; (as-underline fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 下划线控制序列（开启 4、关闭 24）的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (as-underline "underline")) => "\x1b;[4munderline\x1b;[24m")
(check (show #f (as-underline "a" "b")) => "\x1b;[4mab\x1b;[24m")
(check (show #f (as-underline)) => "\x1b;[4m\x1b;[24m")

(check-report)
