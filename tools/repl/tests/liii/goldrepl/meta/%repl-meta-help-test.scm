;; %repl-meta-help
;; 返回元命令帮助信息的行列表。
;;
;; 语法
;; ----
;; (%repl-meta-help)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; list
;; 帮助信息的行列表，第一行是标题，之后每行对应一个元命令。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl meta)
) ;import

(check-set-mode! 'report-failed)

(let ((lines (%repl-meta-help)))
  ;; 帮助信息是非空列表
  (check-true (pair? lines))
  ;; 首行是标题
  (check-true (string-contains (car lines) "Meta commands:"))
  ;; 每个元命令都有一行说明
  (check (length lines) => (+ 1 (length *repl-meta-commands*)))
  ;; 说明行中包含命令名
  (check-true (any (lambda (l) (string-contains l ",quit")) lines))
  (check-true (any (lambda (l) (string-contains l ",apropos")) lines))
) ;let

(check-report)
