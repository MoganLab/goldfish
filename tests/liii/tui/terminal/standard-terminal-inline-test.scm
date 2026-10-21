(import (scheme base)
  (liii check)
  (liii tui terminal)
) ;import

(check-set-mode! 'report)

;; 1. 测试 inline 模式下的相对光标移动
(let* ((p (open-output-string))
       (term (make-standard-terminal :output-port p :inline #t))
      ) ;let*
  (check-true (standard-terminal? term))
  (check (terminal-cursor term) => '(0 0))

  ;; 从 (0, 0) 移动到未渲染的 (0, 2)：开辟新行输出 \r\n\r\n
  (terminal-set-cursor! term 0 2)
  (check (get-output-string p) => "\r\n\r\n")
  (check (terminal-cursor term) => '(0 2))

  ;; 从 (0, 2) 向上移动到 (4, 1)：相对上移 1 行，再右移 4 列
  (terminal-set-cursor! term 4 1)
  (check (terminal-cursor term) => '(4 1))
  (check (get-output-string p) => "\r\n\r\n\x1b;[1A\r\x1b;[4C")
) ;let*

;; 2. 非 inline 模式仍为绝对定位
(let* ((p (open-output-string))
       (term (make-standard-terminal :output-port p))
      ) ;let*
  (terminal-set-cursor! term 4 1)
  (check (get-output-string p) => "\x1b;[2;5H")
  (check (terminal-cursor term) => '(4 1))
) ;let*

(check-report)
