;; each-in-list
;; 列表版顺序输出组合子：逐个输出列表中的元素。
;;
;; 语法
;; ----
;; (each-in-list ls)
;;
;; 参数
;; ----
;; ls : list?
;; 待输出的元素列表。
;;
;; 返回值
;; ----
;; format?
;; 按从左到右顺序输出全部元素的格式化组合子。
;;
;; 说明
;; ----
;; each 接收变长参数，本函数接收单个列表；非列表值按
;; 空序列处理。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (each-in-list (list "a" "b"))) => "ab")
(check (show #f (each-in-list '())) => "")
(check (show #f (each-in-list (list 1 #\x))) => "1x")

(check-report)
