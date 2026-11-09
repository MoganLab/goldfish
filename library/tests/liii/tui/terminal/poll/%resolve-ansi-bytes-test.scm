;; %resolve-ansi-bytes
;; 解析 ANSI 字节流，识别终端按键或 SGR 鼠标事件。
;;
;; 语法
;; ----
;; (%resolve-ansi-bytes bytes)
;;
;; 参数
;; ----
;; bytes : list?
;; ANSI 字节数字列表。
;;
;; 返回值
;; ----
;; any
;; 解析得到的按键符号、字符或 mouse-event 对象。

(import (scheme base)
  (liii check)
  (liii tui mouse)
  (liii tui keys)
  (liii tui terminal)
  (liii tui terminal poll)
) ;import

(check-set-mode! 'report-failed)

;; %resolve-ansi-bytes 层：SGR 鼠标字节流应解析为 mouse-event
;; ESC[<0;10;5M = 27 91 60 48 59 49 48 59 53 77
(check (mouse-event? (%resolve-ansi-bytes '(27 91 60 48 59 49 48 59 53 77)))
  =>
  #t
) ;check
(let ((e (%resolve-ansi-bytes '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type e) => 'press)
  (check (mouse-event-button e) => 'left)
  (check (mouse-event-x e) => 9)
  (check (mouse-event-y e) => 4)
) ;let

;; 滚轮：ESC[<65;3;3M = 27 91 60 54 53 59 51 59 51 77
(let ((e (%resolve-ansi-bytes '(27 91 60 54 53 59 51 59 51 77))))
  (check (mouse-event-type e) => 'wheel-down)
) ;let

;; 普通按键仍解析为符号
(check (%resolve-ansi-bytes '(27 91 65)) => 'up)

;; Meta 前缀与普通按键
(check (%resolve-ansi-bytes (list 27 27 91 65)) => 'alt-up)
(check (%resolve-ansi-bytes (list 27 27 91 49 59 51 65)) => 'alt-up)
(check (%resolve-ansi-bytes (list 27 27)) => 'escape)

(check-report)
