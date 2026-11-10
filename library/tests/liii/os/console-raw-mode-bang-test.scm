;; console-raw-mode!
;; 开启或关闭终端原始模式（raw mode）。
;;
;; 语法
;; ----
;; (console-raw-mode! enable?)
;;
;; 参数
;; ----
;; enable? : boolean?
;; #t 开启 raw mode，#f 恢复原始终端模式。
;;
;; 返回值
;; -----
;; boolean?
;; 成功切换返回 #t，失败返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;; 当 enable? 不是布尔值时抛出错误。
;;
;; 说明
;; ----
;; raw mode 会关闭 ISIG/ICANON/ECHO/IXON，
;; 保证 TUI 能直接读到 Ctrl+C 等控制字节而非收到信号。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 参数类型检查
(check-catch 'type-error (console-raw-mode! "invalid"))
(check-catch 'type-error (console-raw-mode! 123))
(check-catch 'type-error (console-raw-mode! 'symbol))
(check-catch 'type-error (console-raw-mode! '()))

;; 调用返回布尔值
(check-true (boolean? (console-raw-mode! #f)))

;; 开启后恢复：再次关闭必须成功，避免影响后续测试
(let ((prev (console-raw-mode! #t)))
  (check-true (boolean? prev))
  (check-true (console-raw-mode! #f))
) ;let

(check-report)
