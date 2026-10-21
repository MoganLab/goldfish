(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi tui app)
  (liii pi tui run)
) ;import

(check-set-mode! 'report)

;; 1. 测试 pi-app-run 过程定义
(check-true (procedure? pi-app-run))

;; 2. 测试 pi-app-run 在虚拟终端中接收到 ctrl-c 时正常退出
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (virtual-terminal-push-key! vt 'ctrl-c)
  (check (pi-app-run app) => #t)
) ;let*

(check-report)
