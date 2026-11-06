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
;; ixon-disabled / raw-active 键的关联列表。
;;
;; 说明
;; ----
;; 用于验证 raw mode 是否真正关闭了把按键转成信号的位：
;; TUI 依赖读到 Ctrl+C 的 0x03 字节而不是收到 SIGINT。
;; Windows 无 termios，由 C 层统一合成各字段。

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
  ;; 未进入 raw mode 时不断言具体值（可能是任意终端状态），
  ;; 仅验证数据结构；真实断言见下方开启 raw mode 后的检查
) ;let

;; 开启 raw mode 后，ISIG/ICANON/ECHO/IXON 必须全部关闭
(let* ((prev (console-raw-mode! #t)) (state (console-raw-mode-state)))
  (check-true (boolean? prev))
  ;; 仅在确实进入 raw mode 时断言（非 tty 环境如 CI 管道下 raw-active 为 #f）
  (when (eq? (cdr (assoc 'raw-active state)) #t)
    (check (cdr (assoc 'isig-disabled state)) => #t)
    (check (cdr (assoc 'icanon-disabled state)) => #t)
    (check (cdr (assoc 'echo-disabled state)) => #t)
    (check (cdr (assoc 'ixon-disabled state)) => #t)
  ) ;when
  ;; 恢复原终端状态，避免影响后续测试
  (console-raw-mode! #f)
) ;let*

(check-report)
