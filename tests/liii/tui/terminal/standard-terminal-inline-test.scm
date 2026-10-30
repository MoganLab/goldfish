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

  ;; 从 (0, 2) 向上移动到 (4, 1)：CR 先清除待换行状态，再相对上移 1 行、右移 4 列
  (terminal-set-cursor! term 4 1)
  (check (terminal-cursor term) => '(4 1))
  (check (get-output-string p) => "\r\n\r\n\r\x1b;[1A\x1b;[4C")
) ;let*

;; 2. 非 inline 模式为 CR 前置的绝对定位：先清除待换行状态，
;;    保护满宽行末列的待定单元格（Windows conpty 否则丢弃末列字符）
(let* ((p (open-output-string))
       (term (make-standard-terminal :output-port p))
      ) ;let*
  (terminal-set-cursor! term 4 1)
  (check (get-output-string p) => "\r\x1b;[2;5H")
  (check (terminal-cursor term) => '(4 1))
) ;let*

;; 3. 已渲染行内的下移也是 CR 前置：写满末列后的待换行状态
;;    必须先被 \r 清除，再做 ESC[nB 相对下移（Windows 吞字回归）
(let* ((p (open-output-string))
       (term (make-standard-terminal :output-port p :inline #t))
      ) ;let*
  (terminal-set-cursor! term 0 2)
  (terminal-set-cursor! term 0 1)
  (check (get-output-string p) => "\r\n\r\n\r\x1b;[1A")
) ;let*

(check-report)
