(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; terminal-mouse-mode!
;; 开启/关闭终端鼠标事件报告（XTerm 1000/1002/1006 模式）。
;;
;; 语法
;; ----
;; (terminal-mouse-mode! term enable?)
;;
;; 参数
;; ----
;; term : terminal
;; enable? : boolean，#t 开启（发送 1000h/1002h/1006h），#f 关闭（逆序发送 ...l）
;;
;; 返回值
;; ----
;; #t

(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p
                                   :detect-size-fn (lambda () '(100 30)))))
  ;; 开启：按键 + 按钮拖动 + SGR 编码
  (check (terminal-mouse-mode! st #t) => #t)
  (check (get-output-string p) => "\x1b[?1000h\x1b[?1002h\x1b[?1006h")

  ;; 关闭：逆序关闭
  (check (terminal-mouse-mode! st #f) => #t)
  (check (get-output-string p)
    => "\x1b[?1000h\x1b[?1002h\x1b[?1006h\x1b[?1006l\x1b[?1002l\x1b[?1000l")

  ;; flush 后虚拟终端不崩溃
  (check (terminal-flush st) => #t)
) ;let

;; virtual-terminal 同样支持开关（输出进入 raw 流，不崩溃即可）
(let ((vt (make-virtual-terminal 40 20)))
  (check (terminal-mouse-mode! vt #t) => #t)
  (check (terminal-mouse-mode! vt #f) => #t)
  (check (virtual-terminal? vt) => #t)
) ;let

;; 类型检查
(check-catch 'type-error (terminal-mouse-mode! 123 #t))
(check-catch 'type-error (terminal-mouse-mode! "abc" #f))

(check-report)
