;; wrapped/char
;; 按字符折行组合子：宽度不足时在任意字符处断开。
;;
;; 语法
;; ----
;; (wrapped/char . ls)
;;
;; 参数
;; ----
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 每行不超过 width 列、可切断单词的格式化组合子。
;;
;; 说明
;; ----
;; 与 wrapped 的区别是不按单词边界折行，适合长串、
;; base64 等无空格内容。
;; `(liii format columnar)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 columnar))

(check-set-mode! 'report-failed)

(check
  (show #f (with ((width 5)) (wrapped/char "abc def ghij")))
  =>
  "abc d\nef gh\nij\n"
) ;check
(check (show #f (wrapped/char "")) => "")
(check
  (show #f (with ((width 3)) (wrapped/char "ab")))
  =>
  "ab\n"
) ;check

(check-report)
