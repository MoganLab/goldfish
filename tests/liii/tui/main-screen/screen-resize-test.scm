(import (liii check)
        (liii tui terminal)
        (liii tui main-screen buffer)
        (liii tui main-screen driver))

(check-set-mode! 'report-failed)

;; screen-resize! 与 screen-refresh! 测试

(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt)))
  ;; 初始渲染
  (screen-render! screen '("12345" "abcde"))
  (check (screen-lines screen) => '("12345" "abcde"))
  (check (virtual-terminal-screen vt)
         => '("12345     "
              "abcde     "
              "          "))

  ;; 调整尺寸 (resize)
  (screen-resize! screen 15 4)
  (check (terminal-size vt) => '(15 4))
  ;; 验证在新的几何尺寸下自适应全量重绘
  (check (virtual-terminal-screen vt)
         => '("12345          "
              "abcde          "
              "               "
              "               "))

  ;; screen-refresh! 全量刷新测试
  (screen-refresh! screen)
  (check (screen-lines screen) => '("12345" "abcde"))
  (check (virtual-terminal-screen vt)
         => '("12345          "
              "abcde          "
              "               "
              "               "))

  ;; 错误参数测试
  (check-catch 'type-error (screen-resize! screen 0 4))
  (check-catch 'type-error (screen-resize! screen 15 -1))
  (check-catch 'type-error (screen-resize! screen "15" 4)))

(check-report)
