;; screen-auto-hide-cursor-set!
;; 设置主屏幕是否启用自动隐藏光标。
;;
;; 语法
;; ----
;; (screen-auto-hide-cursor-set! screen val)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; val : any
;; 真值表示启用自动隐藏光标。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  (screen-auto-hide-cursor-set! screen #f)
  (check-false (screen-auto-hide-cursor? screen))
  (screen-auto-hide-cursor-set! screen #t)
  (check-true (screen-auto-hide-cursor? screen))
) ;let*

(check-catch 'type-error (screen-auto-hide-cursor-set! "not-a-screen" #f))

(check-report)
