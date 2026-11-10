;; console-raw-mode-state
;; 读取终端当前模式的诊断信息（ISIG/ICANON/ECHO/IXON 是否已关闭等）。
;;
;; 语法
;; ----
;; (console-raw-mode-state)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; alist
;; 返回包含 isig-disabled / icanon-disabled / echo-disabled /
;; ixon-disabled / raw-active / ctrl-c-swallowed / console-available /
;; processed-input-disabled / line-input-disabled / echo-input-disabled 键的关联列表。
;;
;; 说明
;; ----
;; 用于验证 raw mode 是否真正关闭了把按键转成信号的位：
;; TUI 依赖读到 Ctrl+C 的 0x03 字节而不是收到 SIGINT。
;; Windows 下真实读取 GetConsoleMode，并报告常驻 Ctrl+C 处理器状态。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 返回值必须是合法 alist，包含全部诊断字段
(let ((state (console-raw-mode-state)))
  (check-true (list? state))
  (check-true (pair? (assoc 'isig-disabled state)))
  (check-true (pair? (assoc 'icanon-disabled state)))
  (check-true (pair? (assoc 'echo-disabled state)))
  (check-true (pair? (assoc 'ixon-disabled state)))
  (check-true (pair? (assoc 'raw-active state)))
  (check-true (pair? (assoc 'ctrl-c-swallowed state)))
  (check-true (pair? (assoc 'console-available state)))
  (check-true (pair? (assoc 'processed-input-disabled state)))
  (check-true (pair? (assoc 'line-input-disabled state)))
  (check-true (pair? (assoc 'echo-input-disabled state)))
  ;; 未进入 raw mode 时不断言具体值（可能是任意终端状态），
  ;; 仅验证数据结构；真实断言见下方开启 raw mode 后的检查
) ;let

;; 开启 raw mode 后，各标志位必须满足 raw 要求
(let* ((prev (console-raw-mode! #t)) (state (console-raw-mode-state)))
  (check-true (boolean? prev))
  ;; 仅在确实进入 raw mode 时断言（非 tty 环境如 CI 管道下 raw-active 为 #f）
  (when (eq? (cdr (assoc 'raw-active state)) #t)
    (check (cdr (assoc 'isig-disabled state)) => #t)
    (check (cdr (assoc 'icanon-disabled state)) => #t)
    (check (cdr (assoc 'echo-disabled state)) => #t)
    (check (cdr (assoc 'ixon-disabled state)) => #t)
    (check (cdr (assoc 'processed-input-disabled state)) => #t)
    (check (cdr (assoc 'line-input-disabled state)) => #t)
    (check (cdr (assoc 'echo-input-disabled state)) => #t)
    (when (os-windows?)
      (check (cdr (assoc 'ctrl-c-swallowed state)) => #t)
      (check (cdr (assoc 'console-available state)) => #t)
    ) ;when
  ) ;when
  ;; 恢复原终端状态，避免影响后续测试
  (console-raw-mode! #f)
  (when (os-windows?)
    (let ((restored (console-raw-mode-state)))
      (check (cdr (assoc 'ctrl-c-swallowed restored)) => #f)
      (check (cdr (assoc 'raw-active restored)) => #f)
    ) ;let
  ) ;when
) ;let*

(check-report)
