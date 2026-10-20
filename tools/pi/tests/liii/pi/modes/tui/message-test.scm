(import (liii check)
  (liii error)
  (liii tui text-width)
  (liii pi modes tui message)
) ;import

(check-set-mode! 'report-failed)

;; 1.0 基础构造与角色校验
(let ((m1 (make-pi-tui-message 'user "hello"))
      (m2 (make-pi-tui-message 'assistant "world"))
      (m3 (make-pi-tui-message 'system "info"))
     ) ;
  (check-true (pi-tui-message? m1))
  (check (message-role m1) => 'user)
  (check (message-content m1) => "hello")

  (check-true (pi-tui-message? m2))
  (check (message-role m2) => 'assistant)
  (check (message-content m2) => "world")

  (check-true (pi-tui-message? m3))
  (check (message-role m3) => 'system)
  (check (message-content m3) => "info")
) ;let

;; 规范化 (role . text) 或直接传入 pi-tui-message
(let ((m (normalize-pi-tui-message (cons 'user "ping"))))
  (check-true (pi-tui-message? m))
  (check (message-role m) => 'user)
  (check (message-content m) => "ping")
) ;let

(let* ((orig (make-pi-tui-message 'assistant "pong"))
       (m (normalize-pi-tui-message orig))
      ) ;
  (check (eq? m orig) => #t)
) ;let

;; 非法参数校验
(check-catch 'type-error (make-pi-tui-message 'invalid "text"))
(check-catch 'type-error (make-pi-tui-message 'user 123))
(check-catch 'type-error (normalize-pi-tui-message "bad"))

;; 2.0 角色前缀着色与格式化
;; User: 蓝色前缀 "\x1b;[34;1m> \x1b;[0m" 或简要无 ANSI 文本
;; Assistant: 绿色前缀 "\x1b;[32;1m● \x1b;[0m"
;; System: 灰色前缀 "\x1b;[90m■ \x1b;[0m"
(check-true (string? (message-role-prefix 'user)))
(check-true (string? (message-role-prefix 'assistant)))
(check-true (string? (message-role-prefix 'system)))

;; 3.0 自然折行与多字节中英文字符宽度感知
;; 单行不超宽
(let* ((m (make-pi-tui-message 'user "hello world"))
       (lines (render-message-lines m 20))
      ) ;
  ;; lines 是渲染后的字符串列表
  (check (length lines) => 1)
  ;; 首行包含前缀且不超宽
  (check-true (<= (display-width (car lines)) 20))
) ;let

;; 超宽自动折行（首行带前缀，后续行带与前缀同等显示宽度的缩进空格）
(let* ((m (make-pi-tui-message 'assistant "this is a very long text that must be wrapped"))
       (lines (render-message-lines m 15))
      ) ;
  (check-true (> (length lines) 1))
  ;; 每一行的显示宽度（strip ANSI 后）都不超过 15
  (for-each
    (lambda (l)
      (check-true (<= (display-width l) 15))
    ) ;lambda
    lines
  ) ;for-each
) ;let

;; 中英文混合与多字节字符（UTF-8）折行
(let* ((m (make-pi-tui-message 'user "你好世界，Goldfish Scheme 终端用户界面自然折行排版测试！"))
       (lines (render-message-lines m 20))
      ) ;
  (check-true (> (length lines) 1))
  ;; 所有行显示宽度都 <= 20，绝不撕裂
  (for-each
    (lambda (l)
      (check-true (<= (display-width l) 20))
    ) ;lambda
    lines
  ) ;for-each
) ;let

;; 多行原生换行（包含 \n）
(let* ((m (make-pi-tui-message 'system "line 1\nline 2\nline 3"))
       (lines (render-message-lines m 40))
      ) ;
  (check (length lines) => 3)
) ;let

(check-report)
