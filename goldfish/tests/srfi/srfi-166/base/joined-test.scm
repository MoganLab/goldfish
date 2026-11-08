;; joined
;; 序列连接组合子：按指定元素格式与分隔符依次输出列表元素。
;;
;; 语法
;; ----
;; (joined elt-f ls [sep])
;; (joined/prefix elt-f ls [sep])
;; (joined/suffix elt-f ls [sep])
;; (joined/last elt-f last-f ls [sep])
;; (joined/dot elt-f dot-f ls [sep])
;; (joined/range elt-f start [end [sep]])
;;
;; 参数
;; ----
;; elt-f : format?
;; 每个元素使用的格式化对象（常用 each）。
;;
;; ls : list?
;; 待连接的元素列表（joined/range 不需要）。
;;
;; sep : format?
;; 可选，元素之间的分隔符，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 依次输出全部元素的格式化组合子。
;;
;; 说明
;; ----
;; joined/prefix 在首元素前也输出 sep，joined/suffix 在末元素后
;; 也输出 sep，joined/last 用 last-f 格式化最后一个元素，
;; joined/dot 在遇到 improper list 的尾部时插入 dot-f，
;; joined/range 按整数区间 [start, end) 生成元素。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (joined each '(1 2 3) " ")) => "1 2 3")
(check (show #f (joined each '(1 2 3))) => "123")
(check (show #f (joined each '())) => "")
(check
  (show #f (joined (lambda (x) (trimmed/right 3 x)) '("abcdef" "123456") ":"))
  =>
  "abc:123"
) ;check

(check-report)
