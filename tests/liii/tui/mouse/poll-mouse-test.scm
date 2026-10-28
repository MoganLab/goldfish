(import (liii check)
        (liii tui mouse)
        (liii tui keys)
        (liii tui terminal)
        (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; %resolve-ansi-bytes 层：SGR 鼠标字节流应解析为 mouse-event
;; （Windows C++ 层合成同样的序列，POSIX 由终端直接发送，路径统一）
;; ESC[<0;10;5M = 27 91 60 48 59 49 48 59 53 77
(check (mouse-event? (%resolve-ansi-bytes '(27 91 60 48 59 49 48 59 53 77))) => #t)
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

;; parse-key 集成：完整 SGR 序列返回 mouse-event，剩余部分正确剥离
(let ((e (parse-key "\x1b;[<0;5;3M")))
  (check (mouse-event? e) => #t)
  (check (mouse-event-button e) => 'left)
) ;let
(check (parse-key "\x1b;[<0;5;3Mxyz" #t)
  => (cons (make-mouse-event 'press 'left 4 2) "xyz"))
(check (parse-keys "\x1b;[<0;5;3M\x1b;[<0;6;4m")
  => (list (make-mouse-event 'press 'left 4 2) (make-mouse-event 'release 'left 5 3)))

(check-report)
