(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; virtual-terminal-resize!
;; 动态调整虚拟终端的宽度和高度。
;;
;; 语法
;; ----
;; (virtual-terminal-resize! vt new-width new-height)
;;
;; 参数
;; ----
;; vt : virtual-terminal
;; 虚拟终端实例。
;; new-width : integer
;; 新宽度（列数），必须为正整数。
;; new-height : integer
;; 新高度（行数），必须为正整数。
;;
;; 返回值
;; ----
;; unspecified

;; 缩小尺寸测试（截断与光标重新钳位）
(let ((vt (make-virtual-terminal :width 10 :height 4)))
  (terminal-write vt "0123456789\nabcdefghij")
  (terminal-set-cursor! vt 9 3)
  (virtual-terminal-resize! vt 6 2)
  (check (terminal-size vt) => '(6 2))
  (check (virtual-terminal-screen vt) => '("012345" "abcdef"))
  ;; 原光标 '(9 3) 超出 6x2 边界，被安全钳位到 '(5 1)
  (check (terminal-cursor vt) => '(5 1)))

;; 放大尺寸测试（扩充行与列补空格）
(let ((vt (make-virtual-terminal :width 4 :height 2)))
  (terminal-write vt "1234\nabcd")
  (virtual-terminal-resize! vt 6 3)
  (check (terminal-size vt) => '(6 3))
  (check (virtual-terminal-screen vt) => '("1234  " "abcd  " "      ")))

;; 边界与非法参数
(let ((vt (make-virtual-terminal :width 10 :height 5)))
  (check-catch 'type-error (virtual-terminal-resize! "not-vt" 10 5))
  (check-catch 'type-error (virtual-terminal-resize! vt "10" 5))
  (check-catch 'value-error (virtual-terminal-resize! vt 0 5))
  (check-catch 'value-error (virtual-terminal-resize! vt 10 -2)))

(check-report)
