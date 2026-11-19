;; terminal-flush
;; 刷新终端输出缓冲，把待写内容立即提交到目标设备。
;;
;; 语法
;; ----
;; (terminal-flush term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; boolean?
;; 刷新成功返回 #t。

(import (scheme base) (liii check) (liii string) (liii tui terminal))

(check-set-mode! 'report-failed)

;; virtual-terminal: flush 不改变原始输出记录
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (terminal-write vt "one")
  (check (terminal-flush vt) => #t)
  (check (virtual-terminal-raw vt) => '("one"))
) ;let

;; standard-terminal: flush 输出到端口
(let* ((p (open-output-string)) (st (make-standard-terminal :output-port p)))
  (terminal-write st "hello")
  (check (terminal-flush st) => #t)
  (check (string-contains? (get-output-string p) "hello") => #t)
) ;let*

(check-catch 'type-error (terminal-flush "not-a-terminal"))

(check-report)
