(import (liii go))

;; 带捕获变量的任务（let 环境非空），import 前后分别判定
(define blocker (make-chan 0))
(go (blocker)
  (g_worker-notify-error 'eq-var-before (eq? chan-recv! %worker-chan-recv!)))
(g_worker-notify-error 'main 'done)
