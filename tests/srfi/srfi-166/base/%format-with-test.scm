;; %format-with
;; with 的底层过程：用状态变量绑定表构造局部环境。
;;
;; 语法
;; ----
;; (%format-with bindings computation)
;;
;; 参数
;; ----
;; bindings : list?
;; 由 `(cons var val)` 组成的表，var 必须是状态变量本身。
;;
;; computation : format?
;; 在绑定环境下求值的组合子。
;;
;; 返回值
;; ----
;; format?
;; 仅在本层使用新绑定、外层环境不受影响的组合子。
;;
;; 说明
;; ----
;; 这是 with 语法展开后的过程形式，供需要动态构造绑定表的
;; 宏与库函数使用。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check
  (show #f (%format-with (list (cons pad-char #\-)) (padded 4 "ab")))
  =>
  "--ab"
) ;check
(check (show #f (%format-with '() (each "x"))) => "x")

(check-report)
