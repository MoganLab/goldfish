;; terminal-end-sync!
;; 结束终端同步输出（DECRST 2026），提交同步块内的全部写入。
;;
;; 语法
;; ----
;; (terminal-end-sync! term)
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

;; 1. virtual-terminal 捕获同步结束序列
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-end-sync! vt)
  (check (virtual-terminal-raw vt) => '("\x1b;[?2026l"))
) ;let

;; 2. standard-terminal 输出 DECSET 2026 结束序列
(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (terminal-end-sync! st)
  (check (string-contains? (get-output-string p) "\x1b;[?2026l") => #t)
) ;let*

;; 3. 类型错误检查
(check-catch 'type-error (terminal-end-sync! "not-a-terminal"))
(check-catch 'type-error (terminal-end-sync! 12345))

(check-report)
