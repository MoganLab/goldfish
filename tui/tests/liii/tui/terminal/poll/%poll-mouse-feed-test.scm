;; %poll-mouse-feed
;; 测试接缝：注入字节流以驱动鼠标 click 合成逻辑。
;;
;; 语法
;; ----
;; (%poll-mouse-feed byte-list)
;;
;; 参数
;; ----
;; byte-list : list?
;; ANSI 字节数字列表。
;;
;; 返回值
;; ----
;; any
;; 解析得到的按键符号、字符或 mouse-event 对象。

(import (scheme base) (liii check) (liii tui mouse) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; ESC[<0;10;5M = 27 91 60 48 59 49 48 59 53 77（左键按下）
(let ((ev (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event? ev) => #t)
  (check (mouse-event-type ev) => 'press)
  (check (mouse-event-button ev) => 'left)
) ;let

;; ESC[<0;10;5m = 27 91 60 48 59 49 48 59 53 109（同位置同按钮释放 -> 合成 click）
(let ((ev (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
  (check (mouse-event? ev) => #t)
  (check (mouse-event-type ev) => 'click)
  (check (mouse-event-button ev) => 'left)
) ;let

(check-report)
