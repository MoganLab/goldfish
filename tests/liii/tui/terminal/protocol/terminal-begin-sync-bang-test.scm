;; terminal-begin-sync!
;; 开启终端同步输出（DECSET 2026），使随后的多次写入合并为一次刷新。
;;
;; 语法
;; ----
;; (terminal-begin-sync! term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii string) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 1. virtual-terminal 捕获同步开始序列
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-begin-sync! vt)
  (check (virtual-terminal-raw vt) => '("\x1b;[?2026h"))
) ;let

;; 2. standard-terminal 输出 DECSET 2026 开启序列
(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (terminal-begin-sync! st)
  (check (string-contains? (get-output-string p) "\x1b;[?2026h") => #t)
) ;let*

;; 3. 类型错误检查
(check-catch 'type-error (terminal-begin-sync! "not-a-terminal"))
(check-catch 'type-error (terminal-begin-sync! 12345))

(check-report)
