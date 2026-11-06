;; joined/range
;; 区间连接组合子：按整数区间生成并连接元素。
;;
;; 语法
;; ----
;; (joined/range elt-f start [end [sep]])
;;
;; 参数
;; ----
;; elt-f : format?
;; 每个整数元素使用的格式化对象。
;;
;; start : exact-integer?
;; 区间起点（含）。
;;
;; end : exact-integer?
;; 可选，区间终点（不含）；省略时必须由外部组合子截断输出。
;;
;; sep : format?
;; 可选，元素之间的分隔符，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 依次输出区间内整数的格式化组合子。
;;
;; 说明
;; ----
;; 元素是按需生成的，因此必须给出 end，否则区间无限延伸。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (joined/range each 1 4 ",")) => "1,2,3")
(check (show #f (joined/range each 1 3)) => "12")
(check (show #f (joined/range each 3 3)) => "")

(check-report)
