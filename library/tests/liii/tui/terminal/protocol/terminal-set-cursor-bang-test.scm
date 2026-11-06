;; terminal-set-cursor!
;; 设置终端的光标位置。
;;
;; 语法
;; ----
;; (terminal-set-cursor! term x y)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; x : integer?
;; 目标列坐标（0-based）。
;;
;; y : integer?
;; 目标行坐标（0-based）。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (terminal-set-cursor! vt 5 2)
  (check (terminal-cursor vt) => '(5 2))
  ;; 在移动后的位置写入
  (terminal-write vt "ok")
  (check (terminal-cursor vt) => '(7 2))
  (check (list-ref (virtual-terminal-screen vt) 2) => "     ok             ")
  ;; 超出边界自动钳位
  (terminal-set-cursor! vt -5 -10)
  (check (terminal-cursor vt) => '(0 0))
  (terminal-set-cursor! vt 100 100)
  (check (terminal-cursor vt) => '(19 4))
) ;let

;; 类型校验
(check-catch 'type-error (terminal-set-cursor! "not-term" 0 0))
(check-catch 'type-error
  (terminal-set-cursor! (make-virtual-terminal :width 10 :height 5) "0" 0)
) ;check-catch
(check-catch 'type-error
  (terminal-set-cursor! (make-virtual-terminal :width 10 :height 5) 0 "0")
) ;check-catch

(check-report)
