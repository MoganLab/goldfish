;; screen-terminal
;; 获取主屏幕驱动关联的底层终端实例。
;;
;; 语法
;; ----
;; (screen-terminal screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; terminal?
;; 关联的终端实例。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  (check (eq? (screen-terminal screen) vt) => #t)
) ;let*

(check-catch 'type-error (screen-terminal "not-a-screen"))

(check-report)
