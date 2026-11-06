;; alt-screen-terminal
;; 获取备用屏关联的底层终端实例。
;;
;; 语法
;; ----
;; (alt-screen-terminal screen)
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; 返回值
;; ----
;; terminal?
;; 关联的终端实例。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5)) (screen (make-alt-screen vt)))
  (check (alt-screen-terminal screen) => vt)
) ;let*

(check-catch 'type-error (alt-screen-terminal "not-a-screen"))

(check-report)
