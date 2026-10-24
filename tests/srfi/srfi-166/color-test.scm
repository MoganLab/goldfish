(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 color))

(check-set-mode! 'report-failed)

;; (srfi srfi-166 color) 模块函数分类索引
;;
;; color 模块提供基于 ANSI 转义码的终端彩色输出与样式修饰组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc srfi/srfi-166/color "as-red"
;;   bin/gf doc srfi/srfi-166/color "on-blue"
;;   bin/gf doc srfi/srfi-166/color "as-bold"
;;   bin/gf doc srfi/srfi-166/color "as-color"

(check (show #f (as-red "red")) => "\x1b[31mred\x1b[39m")
(check (show #f (on-blue "blue")) => "\x1b[44mblue\x1b[49m")
(check (show #f (as-bold "bold")) => "\x1b[1mbold\x1b[22m")

(check-report)
