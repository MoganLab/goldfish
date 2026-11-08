;; as-bold
;; 终端文本修饰组合子。为输出内容添加粗体样式，并在结束时恢复。
;;
;; 语法
;; ----
;; (as-bold fmt ...)
;;
;; 参数
;; ----
;; fmt : format?
;; 零个或多个格式化对象（字符串或其他格式化组合子）。
;;
;; 返回值
;; ----
;; format?
;; 包裹了 ANSI 粗体控制序列（开启 1、关闭 22）的格式化组合子。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 color))

(check-set-mode! 'report-failed)

(check (show #f (as-bold "bold")) => "\x1b;[1mbold\x1b;[22m")
(check
  (show #f (as-red (each (as-underline (as-bold "abc") "def") "ghi")))
  =>
  "\x1b;[31m\x1b;[4m\x1b;[1mabc\x1b;[22mdef\x1b;[24mghi\x1b;[39m"
) ;check

(check-report)
