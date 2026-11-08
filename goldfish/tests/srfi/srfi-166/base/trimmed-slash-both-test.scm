;; trimmed/both
;; 两侧截断组合子：内容超宽时保留中间。
;;
;; 语法
;; ----
;; (trimmed/both width . ls)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 允许的最大显示宽度。
;;
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 输出不超过 width 列、保留中间内容的格式化组合子。
;;
;; 说明
;; ----
;; 省略号会同时加在被裁掉的两端，且计入 width。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (trimmed/both 4 "abcdefg")) => "bcde")
(check (show #f (trimmed/both 10 "abc")) => "abc")
(check
  (show #f (with ((ellipsis "..")) (trimmed/both 6 "abcdefghij")))
  =>
  "..ef.."
) ;check

(check-report)
