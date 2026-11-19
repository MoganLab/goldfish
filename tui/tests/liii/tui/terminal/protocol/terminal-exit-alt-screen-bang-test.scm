;; terminal-exit-alt-screen!
;; 退出备用屏（alt screen），恢复进入前的主屏内容。
;;
;; 语法
;; ----
;; (terminal-exit-alt-screen! term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; #t
;; 操作成功返回 #t。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 标准终端：退出下发 ?1049l
(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (terminal-enter-alt-screen! st)
  (check (terminal-exit-alt-screen! st) => #t)
  (check (get-output-string p) => "\x1b;[?1049h\x1b;[?1049l")
) ;let*

;; 虚拟终端同样支持（输出进入 raw 流）
(let ((vt (make-virtual-terminal :width 40 :height 20)))
  (check (terminal-exit-alt-screen! vt) => #t)
  (check-true (virtual-terminal? vt))
) ;let

;; 类型检查
(check-catch 'type-error (terminal-exit-alt-screen! "not-a-term"))

(check-report)
