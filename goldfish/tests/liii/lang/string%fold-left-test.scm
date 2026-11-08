;; string%fold-left
;; 字符串左折叠（对标 Scala String.foldLeft）。
;;
;; 语法
;; ----
;; ($ s :fold-left init f)
;;
;; 参数
;; ----
;; f : procedure?
;; 二参函数 (acc char) => 新累加器。UTF-8 语义。
;;
;; 返回值
;; ------
;; any
;; 最终累加器；空串返回 init。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "abc" :fold-left 0 (lambda (acc c) (+ acc 1))) => 3)
(check ($ "测试" :fold-left 0 (lambda (acc c) (+ acc 1))) => 2)
(check ($ "" :fold-left 9 (lambda (acc c) acc)) => 9)

(check-report)
