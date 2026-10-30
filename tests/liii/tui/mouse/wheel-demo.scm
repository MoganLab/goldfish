;; tmux 验收：鼠标滚轮方向（SGR 1006 解析 + 滚动消费链路）
;;
;; 用法：在 tmux 会话内运行
;;   tmux set -g mouse on
;;   bin/gf tests/liii/tui/mouse/tmux-wheel-demo.scm
;; 然后滚动鼠标滚轮（或 tmux send-keys -t <session> -M WheelUpPane），
;; 屏幕实时打印事件：wheel-up=向上翻，wheel-down=向下翻。
;; 按 q 或 Ctrl+C 退出。
(import (scheme base)
        (scheme write)
        (liii tui terminal)
        (liii tui mouse))

(define term (make-standard-terminal))
(define n 0)
(define row 0)

(define (show! text)
  ;; raw 模式下手动回到行首并补 \r\n；限制在屏幕前 20 行内滚动打印
  (terminal-write term "\r\n")
  (terminal-write term text)
  (terminal-flush term))

(dynamic-wind
  (lambda ()
    (terminal-enter-raw-mode! term)
    (terminal-mouse-mode! term #t)
    (terminal-write term "\r\nwheel demo: scroll mouse wheel, q to quit\r\n")
    (terminal-flush term))
  (lambda ()
    (let loop ()
      (let ((ev (terminal-poll-key term)))
        (cond
         ((eq? ev 'ctrl-c)
          'exit)
         ((and (string? ev) (string=? ev "q"))
          'exit)
         ((mouse-event? ev)
          (set! n (+ n 1))
          (show! (string-append
                   (number->string n) ": "
                   (symbol->string (mouse-event-type ev)) " y="
                   (number->string (mouse-event-y ev))))
          (loop))
         (else (loop))))))
  (lambda ()
    (terminal-mouse-mode! term #f)
    (terminal-exit-raw-mode! term)))
