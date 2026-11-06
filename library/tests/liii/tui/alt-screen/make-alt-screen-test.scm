;; make-alt-screen
;; 创建备用屏（alt screen）驱动实例。
;;
;; 语法
;; ----
;; (make-alt-screen term)
;;
;; 参数
;; ----
;; term : terminal?
;; 底层终端实例。
;;
;; 返回值
;; ----
;; alt-screen?
;; 创建的备用屏实例。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5)) (screen (make-alt-screen vt)))
  (check-true (alt-screen? screen))
  (check (alt-screen-terminal screen) => vt)
  (check (alt-screen-lines screen) => '())
) ;let*

(check-catch 'type-error (make-alt-screen "not-a-terminal"))
(check-catch 'type-error (make-alt-screen 123))

(check-report)
