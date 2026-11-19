;; terminal-clear
;; 清空整个终端表面并将光标复位到左上角。
;;
;; 语法
;; ----
;; (terminal-clear term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (terminal-set-cursor! vt 1 1)
  (terminal-write vt "data")
  (terminal-clear vt)
  (check (terminal-cursor vt) => '(0 0))
  (check (car (virtual-terminal-screen vt)) => (make-string 20 #\space))
  (check (cadr (virtual-terminal-screen vt)) => (make-string 20 #\space))
) ;let

(check-catch 'type-error (terminal-clear "not-a-terminal"))

(check-report)
