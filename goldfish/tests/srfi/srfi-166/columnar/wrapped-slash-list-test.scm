;; wrapped/list
;; 词表折行组合子：把已切分好的单词列表按宽度折行。
;;
;; 语法
;; ----
;; (wrapped/list ls)
;;
;; 参数
;; ----
;; ls : list?
;; 单词（或任意可显示对象）组成的列表。
;;
;; 返回值
;; ----
;; format?
;; 每行尽量装满 width 列、行内用 pad-char 分隔的组合子。
;;
;; 说明
;; ----
;; 本函数不做分词，分词由 wrapped 负责；行内分隔符取状态
;; 变量 pad-char（默认空格）。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check (show #f (wrapped/list '("aa" "bb" "cc"))) => "aa bb cc")
(check
  (show #f (with ((width 4)) (wrapped/list '("aa" "bb" "cc"))))
  =>
  "aa\nbb\ncc"
) ;check
(check (show #f (wrapped/list '())) => "")

(check-report)
