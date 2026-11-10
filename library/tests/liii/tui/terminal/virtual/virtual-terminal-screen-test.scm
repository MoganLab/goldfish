;; virtual-terminal-screen
;; 获取虚拟终端当前的屏幕内容（按行的字符串列表）。
;;
;; 语法
;; ----
;; (virtual-terminal-screen vt)
;;
;; 参数
;; ----
;; vt : virtual-terminal?
;; 虚拟终端实例。
;;
;; 返回值
;; ----
;; list?
;; 每行一个字符串的屏幕内容列表。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 10 :height 3)))
  ;; 初始为全空格
  (check (virtual-terminal-screen vt)
    =>
    '("          " "          " "          ")
  ) ;check
  (terminal-write vt "hi")
  (check (virtual-terminal-screen vt)
    =>
    '("hi        " "          " "          ")
  ) ;check
) ;let

(check-catch 'type-error (virtual-terminal-screen "not-a-vt"))
(check-catch 'type-error (virtual-terminal-screen 123))

(check-report)
