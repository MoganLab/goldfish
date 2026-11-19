;; terminal-cursor
;; 查询终端当前的光标位置。
;;
;; 语法
;; ----
;; (terminal-cursor term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; pair?
;; 形如 (x . y) 的光标位置点对，坐标从 0 开始。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal-cursor vt) => '(0 0))
  (terminal-set-cursor! vt 5 2)
  (check (terminal-cursor vt) => '(5 2))
) ;let

(check-catch 'type-error (terminal-cursor "not-a-terminal"))

(check-report)
