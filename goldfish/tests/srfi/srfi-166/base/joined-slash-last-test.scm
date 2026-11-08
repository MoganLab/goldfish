;; joined/last
;; 末元素特化连接组合子：最后一个元素用独立格式输出。
;;
;; 语法
;; ----
;; (joined/last elt-f last-f ls [sep])
;;
;; 参数
;; ----
;; elt-f : format?
;; 除末元素外各元素使用的格式化对象。
;;
;; last-f : format?
;; 末元素使用的格式化对象。
;;
;; ls : list?
;; 元素列表。
;;
;; sep : format?
;; 可选，元素之间的分隔符，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 自然语言式列举的格式化组合子。
;;
;; 说明
;; ----
;; 常用于生成 "lions, tigers, and bears" 这类列举。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check
  (show #f (joined/last each (lambda (x) (each "& " x)) '(1 2 3) ", "))
  =>
  "1, 2, & 3"
) ;check
(check (show #f (joined/last each each '(1) ", ")) => "1")
(check (show #f (joined/last each each '() ", ")) => "")

(check-report)
