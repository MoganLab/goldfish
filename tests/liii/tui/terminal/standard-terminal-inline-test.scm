(import (scheme base)
  (scheme write)
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

  ;; 从 (0, 2) 向上移动到 (0, 1)：相对上移 1 行
  (let ((p2 (open-output-string)))
    (let ((term2 (make-standard-terminal :output-port p2 :inline #t)))
      (terminal-set-cursor! term2 0 2)
      (let ((p3 (open-output-string)))
        (terminal-set-cursor! term 4 1)
        (check (terminal-cursor term) => '(4 1))
      ) ;let
    ) ;let
  ) ;let
) ;let*

(check-report)
