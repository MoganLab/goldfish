;; %repl-editor-help-lines
;; 返回编辑命令帮助信息的行列表。
;;
;; 语法
;; ----
;; (%repl-editor-help-lines)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; list
;; 帮助信息的行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl render)
) ;import

(check-set-mode! 'report-failed)

(let ((lines (%repl-editor-help-lines)))
  ;; 帮助信息是非空列表
  (check-true (pair? lines))
  ;; 首行是标题
  (check-true (any (lambda (l) (string-contains l "Editing Commands")) lines))
  ;; 常用编辑命令都有说明
  (check-true (any (lambda (l) (string-contains l "shift-tab")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-a")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-k")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-c")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-l")) lines))
) ;let

(check-report)
