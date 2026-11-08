;; call-with-output
;; 捕获子组合子的输出，再交给消费者组合子。
;;
;; 语法
;; ----
;; (call-with-output producer consumer)
;;
;; 参数
;; ----
;; producer : format?
;; 被捕获输出的格式化对象。
;;
;; consumer : (string? -> format?)
;; 接收捕获到的字符串、返回新组合子的过程。
;;
;; 返回值
;; ----
;; format?
;; 先运行 producer、再把结果串交给 consumer 的组合子。
;;
;; 说明
;; ----
;; producer 的输出不会直接进入外层端口，因此常用于
;; 先算内容宽度、再决定如何排版。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check
  (show #f (call-with-output (each "in") (lambda (s) (each "<" s ">"))))
  =>
  "<in>"
) ;check
(check
  (show #f (call-with-output (each "abcd") (lambda (s) (each (string-length s)))))
  =>
  "4"
) ;check
(check
  (show #f (call-with-output (each) (lambda (s) (each "[" s "]"))))
  =>
  "[]"
) ;check

(check-report)
