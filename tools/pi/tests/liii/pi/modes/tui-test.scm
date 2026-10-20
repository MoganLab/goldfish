(import (liii check)
  (liii error)
  (liii tui terminal)
  (liii pi modes tui)
) ;import

(check-set-mode! 'report-failed)

;; 1. with-pi-tui-terminal 参数类型检查
(check-catch 'type-error (with-pi-tui-terminal "invalid" (lambda () #t)))
(check-catch 'type-error (with-pi-tui-terminal (make-virtual-terminal) "invalid"))
(check-catch 'type-error (run-tui-loop "invalid"))

;; 2. with-pi-tui-terminal 正常生命周期与终端状态还原
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (during-visible #t)
     ) ;
  (check (terminal-cursor-visible? vt) => #t)
  (with-pi-tui-terminal vt
    (lambda ()
      (set! during-visible (terminal-cursor-visible? vt))
    ) ;lambda
  ) ;with-pi-tui-terminal
  ;; 进入时隐藏光标，退出后恢复显示
  (check during-visible => #f)
  (check (terminal-cursor-visible? vt) => #t)
  ;; 验证写入了 alternate screen 进入与退出序列
  (let ((raw (virtual-terminal-raw vt)))
    (check-true (not (not (member "\x1b;[?1049h" raw))))
    (check-true (not (not (member "\x1b;[?1049l" raw))))
  ) ;let
) ;let

;; 3. with-pi-tui-terminal 异常情况下的 100% 安全还原保证
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (caught? #f)
     ) ;
  (catch #t
    (lambda ()
      (with-pi-tui-terminal vt
        (lambda ()
          (error 'test-error "something went wrong inside tui")
        ) ;lambda
      ) ;with-pi-tui-terminal
    ) ;lambda
    (lambda (k . args)
      (set! caught? #t)
    ) ;lambda
  ) ;catch
  (check caught? => #t)
  ;; 即使内部发生异常，光标也必须恢复显示，并且退出 alternate screen
  (check (terminal-cursor-visible? vt) => #t)
  (let ((raw (virtual-terminal-raw vt)))
    (check-true (not (not (member "\x1b;[?1049l" raw))))
  ) ;let
) ;let

;; 4. run-tui-loop 响应 'escape 优雅退出
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (check (run-tui-loop vt) => 0)
) ;let

;; 5. run-tui-loop 响应 "q" 优雅退出
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt "q")
  (check (run-tui-loop vt) => 0)
) ;let

;; 6. run-tui-loop 响应 "Q" 优雅退出
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt "Q")
  (check (run-tui-loop vt) => 0)
) ;let

;; 7. run-tui-loop 忽略普通按键并在遇到退出键时退出
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (received-keys '())
     ) ;
  (virtual-terminal-push-key! vt "x")
  (virtual-terminal-push-key! vt "y")
  (virtual-terminal-push-key! vt 'escape)
  (let ((ret (run-tui-loop vt :on-key (lambda (k) (set! received-keys (cons k received-keys))))))
    (check ret => 0)
    (check (reverse received-keys) => '("x" "y" escape))
  ) ;let
) ;let

;; 8. run-tui-flow 端到端集成验证
(let ((vt (make-virtual-terminal :width 80 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (let ((ret (run-tui-flow '() '() 'tui "" vt)))
    (check ret => 0)
    (check (terminal-cursor-visible? vt) => #t)
    (let ((raw (virtual-terminal-raw vt)))
      (check-true (not (not (member "\x1b;[?1049l" raw))))
    ) ;let
  ) ;let
) ;let

(check-report)
