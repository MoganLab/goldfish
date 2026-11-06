;; wrapped
;; 按单词边界自动折行组合子。
;;
;; 语法
;; ----
;; (wrapped . ls)
;;
;; 参数
;; ----
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 每行不超过 width 列、按单词换行的格式化组合子。
;;
;; 说明
;; ----
;; 先用状态变量 word-separator? 切分词，再交给 wrapped/list
;; 排版；首行从当前列号起算，因此行首空白会被丢弃。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check (show #f (wrapped "foo")) => "foo")
(check (show #f (wrapped "    ")) => "")
(check
  (show #f (with ((width 8)) (wrapped "hello world")))
  =>
  "hello\nworld"
) ;check
(check
  (show #f (with ((width 8)) (wrapped "abcdefg hi")))
  =>
  "abcdefg\nhi"
) ;check

(check-report)
