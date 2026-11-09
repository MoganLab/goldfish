;; trimmed/lazy
;; 惰性截断组合子：不展开内容，直接丢弃超出宽度的部分。
;;
;; 语法
;; ----
;; (trimmed/lazy width . ls)
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
;; 输出不超过 width 列内容的格式化组合子。
;;
;; 说明
;; ----
;; 本组合子不计算内容真实宽度，也不处理省略号，
;; 适合宽度已知、只想截断字节流的场景。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (trimmed/lazy 3 "abcdef")) => "abc")
(check (show #f (trimmed/lazy 10 "abc")) => "abc")
(check (show #f (trimmed/lazy 0 "abc")) => "")

(check-report)
