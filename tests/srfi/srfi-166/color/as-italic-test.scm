;; as-italic
;; 终端文本修饰组合子。为输出内容添加斜体样式，并在结束时恢复。
;;
;; 语法
;; ----
;; (as-italic fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 斜体控制序列（开启 3、关闭 23）的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (as-italic "italic")) => "\x1b;[3mitalic\x1b;[23m")
(check (show #f (as-italic "a" "b")) => "\x1b;[3mab\x1b;[23m")
(check (show #f (as-italic)) => "\x1b;[3m\x1b;[23m")

(check-report)
