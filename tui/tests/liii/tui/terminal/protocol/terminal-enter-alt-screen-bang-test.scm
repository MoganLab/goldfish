;; terminal-enter-alt-screen!
;; 进入备用屏（alt screen），切换到独立的全屏缓冲区。
;;
;; 语法
;; ----
;; (terminal-enter-alt-screen! term)
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

;; 标准终端：进入下发 ?1049h
(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (check (terminal-enter-alt-screen! st) => #t)
  (check (get-output-string p) => "\x1b;[?1049h")
) ;let*

;; 虚拟终端同样支持（输出进入 raw 流）
(let ((vt (make-virtual-terminal :width 40 :height 20)))
  (check (terminal-enter-alt-screen! vt) => #t)
  (check-true (virtual-terminal? vt))
) ;let

;; 类型检查
(check-catch 'type-error (terminal-enter-alt-screen! 123))

(check-report)
