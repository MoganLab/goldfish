(import (liii check)
        (liii tui terminal)
        (liii tui main-screen buffer)
        (liii tui main-screen driver))

(check-set-mode! 'report-failed)

;; make-main-screen 与属性访问测试

(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-main-screen vt)))
  (check (main-screen? screen) => #t)
  (check (eq? (screen-terminal screen) vt) => #t)
  (check (screen-buffer? (screen-buffer screen)) => #t)
  (check (screen-lines screen) => '())
  (check (screen-auto-hide-cursor? screen) => #t)

  ;; 修改 auto-hide-cursor
  (screen-auto-hide-cursor-set! screen #f)
  (check (screen-auto-hide-cursor? screen) => #f)
  (screen-auto-hide-cursor-set! screen #t)
  (check (screen-auto-hide-cursor? screen) => #t))

;; 带参数创建
(let* ((vt (make-virtual-terminal :width 30 :height 10))
       (screen (make-main-screen vt :auto-hide-cursor #f)))
  (check (main-screen? screen) => #t)
  (check (screen-auto-hide-cursor? screen) => #f))

(let* ((vt (make-virtual-terminal :width 30 :height 10))
       (screen (make-main-screen vt :hide-cursor #f)))
  (check (main-screen? screen) => #t)
  (check (screen-auto-hide-cursor? screen) => #f))

;; 错误处理
(check-catch 'type-error (make-main-screen "not-a-terminal"))
(check-catch 'type-error (make-main-screen 123))
(check-catch 'type-error
  (make-main-screen (make-virtual-terminal :width 10 :height 5) :invalid-option 1))

(check-report)
