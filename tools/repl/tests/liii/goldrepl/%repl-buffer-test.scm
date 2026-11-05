;; %repl-buffer
;; 创建多行输入缓冲区，初始内容为空行列表。
;;
;; 语法
;; ----
;; (%repl-buffer)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; list
;; 长度为 1 的列表，首元素是当前的多行输入文本。

(import (scheme base) (liii check) (liii list) (liii goldrepl))

(check-set-mode! 'report-failed)

;; 缓冲区是一个列表
(let ((buf (%repl-buffer)))
  (check (list? buf) => #t)
  ;; 初始内容是空行
  (check (length buf) => 1)
  (check (car buf) => "")
) ;let

;; 每次调用都会创建新的缓冲区
(check (eq? (%repl-buffer) (%repl-buffer)) => #f)

(check-report)
