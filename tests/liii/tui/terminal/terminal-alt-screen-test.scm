(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; terminal-enter-alt-screen! / terminal-exit-alt-screen!
;; 进入/退出备用屏（alt screen）：切换到独立的全屏缓冲区，
;; 退出时恢复进入前的主屏内容。
;;
;; 语法
;; ----
;; (terminal-enter-alt-screen! term)
;; (terminal-exit-alt-screen! term)
;;
;; 参数
;; ----
;; term : terminal
;; 终端实例。
;;
;; 返回值
;; ----
;; #t

;; 标准终端：进入下发 ?1049h，退出下发 ?1049l
(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p)))
  (check (terminal-enter-alt-screen! st) => #t)
  (check (get-output-string p) => "\x1b[?1049h")
  (check (terminal-exit-alt-screen! st) => #t)
  (check (get-output-string p) => "\x1b[?1049h\x1b[?1049l")
) ;let

;; 虚拟终端同样支持（输出进入 raw 流）
(let ((vt (make-virtual-terminal :width 40 :height 20)))
  (check (terminal-enter-alt-screen! vt) => #t)
  (check (terminal-exit-alt-screen! vt) => #t)
  (check (virtual-terminal? vt) => #t)
) ;let

;; 类型检查
(check-catch 'type-error (terminal-enter-alt-screen! 123))
(check-catch 'type-error (terminal-exit-alt-screen! "not-a-term"))

(check-report)
