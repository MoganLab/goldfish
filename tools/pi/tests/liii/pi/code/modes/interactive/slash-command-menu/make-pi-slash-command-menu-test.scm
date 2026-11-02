;; make-pi-slash-command-menu
;; 根据候选命令列表构造斜杠命令下拉选择组件。
;;
;; 语法
;; ----
;; (make-pi-slash-command-menu commands)
;;
;; 参数
;; ----
;; commands : list?
;; 候选命令列表。
;;
;; 返回值
;; ----
;; component?
;; 斜杠命令选择列表组件。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive slash-command-menu)
) ;import

(check-set-mode! 'report-failed)

;; 1. 根据命令列表构造下拉菜单组件
(let* ((commands (pi-slash-command-matches "/"))
       (menu (make-pi-slash-command-menu commands))
      ) ;
  (check-true (component? menu))
  (let ((lines (component-render menu 80)))
    (check-true (pair? lines))
  ) ;let
) ;let*

(check-report)
