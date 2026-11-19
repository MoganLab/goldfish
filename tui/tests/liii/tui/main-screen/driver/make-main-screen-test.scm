;; make-main-screen
;; 创建一个主屏幕（inline 场景）驱动实例。
;;
;; 语法
;; ----
;; (make-main-screen term [:auto-hide-cursor flag] [:hide-cursor flag])
;;
;; 参数
;; ----
;; term : terminal?
;; 底层终端实例。
;;
;; flag : boolean? (可选，默认为 #t)
;; :auto-hide-cursor 与 :hide-cursor 均用于设置是否自动隐藏光标。
;;
;; 返回值
;; ----
;; main-screen?
;; 创建的主屏幕驱动实例。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen buffer)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

;; 默认参数
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt))
      ) ;
  (check-true (main-screen? screen))
  (check (screen-lines screen) => '())
  (check-true (screen-auto-hide-cursor? screen))
) ;let*

;; 带参数创建
(let* ((vt (make-virtual-terminal :width 30 :height 10))
       (screen (make-main-screen vt :auto-hide-cursor #f))
      ) ;
  (check-true (main-screen? screen))
  (check-false (screen-auto-hide-cursor? screen))
) ;let*

(let* ((vt (make-virtual-terminal :width 30 :height 10))
       (screen (make-main-screen vt :hide-cursor #f))
      ) ;
  (check-true (main-screen? screen))
  (check-false (screen-auto-hide-cursor? screen))
) ;let*

;; 错误处理
(check-catch 'type-error (make-main-screen "not-a-terminal"))
(check-catch 'type-error (make-main-screen 123))
(check-catch 'type-error
  (make-main-screen (make-virtual-terminal :width 10 :height 5) :invalid-option 1)
) ;check-catch

(check-report)
