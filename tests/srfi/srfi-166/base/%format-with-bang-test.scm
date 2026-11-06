;; %format-with!
;; with! 的底层过程：用绑定表就地修改当前环境。
;;
;; 语法
;; ----
;; (%format-with! bindings)
;;
;; 参数
;; ----
;; bindings : list?
;; 由 `(cons var val)` 组成的表，var 必须是状态变量本身。
;;
;; 返回值
;; ----
;; format?
;; 只执行状态修改、不输出内容的组合子。
;;
;; 说明
;; ----
;; 这是 with! 语法展开后的过程形式，修改对后续组合子持续
;; 生效，与 %format-with 的局部绑定相对。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check
  (show #f (%format-with! (list (cons pad-char #\-))) (padded 4 "ab"))
  =>
  "--ab"
) ;check
(check (show #f (%format-with! '()) (each "x")) => "x")

(check-report)
