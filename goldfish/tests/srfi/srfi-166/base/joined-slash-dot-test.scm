;; joined/dot
;; 点对连接组合子：为 improper list 的尾部使用独立格式。
;;
;; 语法
;; ----
;; (joined/dot elt-f dot-f ls [sep])
;;
;; 参数
;; ----
;; elt-f : format?
;; 各列表元素使用的格式化对象。
;;
;; dot-f : format?
;; 尾部（非 cons 的 cdr）使用的格式化对象。
;;
;; ls : list?
;; 待连接的列表。
;;
;; sep : format?
;; 可选，元素之间的分隔符，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 输出整个 improper list 的格式化组合子。
;;
;; 说明
;; ----
;; 列表是 proper list 时行为与 joined 相同。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check
  (show #f (joined/dot each (lambda (x) (each " . " x)) '(1 2 . 3) ","))
  =>
  "1,2, . 3"
) ;check
(check (show #f (joined/dot each each '(1 2 3) ",")) => "1,2,3")

(check-report)
