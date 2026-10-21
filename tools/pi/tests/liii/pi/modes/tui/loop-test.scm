(import (liii check)
  (liii error)
  (liii tui main-screen)
  (liii tui terminal)
  (liii pi modes tui state)
  (liii pi modes tui loop)
) ;import

(check-set-mode! 'report-failed)

;; 1. 参数类型检查
(check-catch 'type-error (run-tui-loop "not-terminal"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :state "not-state"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :render "not-proc"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :on-key "not-proc"))
(check-catch 'type-error (run-tui-loop (make-virtual-terminal) :on-tick "not-proc"))

;; 2. 当状态处于 'exit 时立即跳出
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (st (make-pi-tui-state :status 'exit))
     ) ;
  (check (run-tui-loop vt :state st) => 0)
) ;let

;; 3. max-ticks 节拍控制与 on-tick 计数
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (tick-count 0)
     ) ;
  (let ((ret (run-tui-loop vt
               :max-ticks 5
               :on-tick (lambda (st ticks)
                          (set! tick-count (+ tick-count 1))
                        ) ;lambda
             ) ;run-tui-loop
        ) ;ret
       ) ;
    (check ret => 0)
    (check tick-count => 5)
  ) ;let
) ;let

;; 4. 脏标记与 render 调度机制（仅当 state-dirty? 时触发）
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (render-count 0)
      (st (make-pi-tui-state :dirty? #t))
     ) ;
  ;; 运行 4 个 tick：
  ;; tick 0: 初始 dirty? 为 #t，触发 render，render-count = 1，dirty? 被清为 #f
  ;; tick 1: dirty? 为 #f，无按键，不触发 render
  ;; tick 2: dirty? 为 #f，无按键，不触发 render
  ;; tick 3: on-tick 将 state 标记为 dirty，但在 tick 结束时标记，下一次循环触发？
  ;; 我们用 on-tick 在 tick 1 时主动调用 state-mark-dirty!
  (run-tui-loop vt
    :state st
    :max-ticks 4
    :render (lambda (current-st term)
              (set! render-count (+ render-count 1))
            ) ;lambda
    :on-tick (lambda (current-st ticks)
               (when (= ticks 1)
                 (state-mark-dirty! current-st)
               ) ;when
             ) ;lambda
  ) ;run-tui-loop
  ;; 第 0 轮初始渲染 1 次，第 2 轮响应 tick 1 的 dirty 渲染 1 次，总共恰好 2 次
  (check render-count => 2)
  (check (state-dirty? st) => #f)
) ;let

;; 5. 按键事件分发与状态流转驱动
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (st (make-pi-tui-state))
      (rendered-statuses '())
     ) ;
  ;; 注入按键流：切换到 running -> 切换到 streaming -> 切换到 confirm -> exit
  (virtual-terminal-push-key! vt "r")
  (virtual-terminal-push-key! vt "s")
  (virtual-terminal-push-key! vt "c")
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt
                    :state st
                    :return-state #t
                    :render (lambda (s term)
                              (set! rendered-statuses (cons (state-status s) rendered-statuses))
                            ) ;lambda
                    :on-key (lambda (s key)
                              (cond ((equal? key "r") (state-with-status s 'running))
                                    ((equal? key "s") (state-with-status s 'streaming))
                                    ((equal? key "c") (state-with-status s 'confirm))
                                    (else s)
                              ) ;cond
                            ) ;lambda
                  ) ;run-tui-loop
        ) ;final-st
       ) ;
    (check (pi-tui-state? final-st) => #t)
    (check (state-status final-st) => 'exit)
    ;; 验证每次状态变更（idle -> running -> streaming -> confirm）都经历了 render 重绘
    (check (reverse rendered-statuses) => '(idle running streaming confirm))
  ) ;let
) ;let

;; 6. 自定义 on-key 拦截 'handled 避免默认退出
(let ((vt (make-virtual-terminal :width 80 :height 24))
      (q-handled-count 0)
     ) ;
  (virtual-terminal-push-key! vt "q")
  (virtual-terminal-push-key! vt "q")
  (virtual-terminal-push-key! vt 'escape)

  (let ((ret (run-tui-loop vt
               :on-key (lambda (s key)
                         (if (equal? key "q")
                           (begin (set! q-handled-count (+ q-handled-count 1)) 'handled)
                           s
                         ) ;if
                       ) ;lambda
             ) ;run-tui-loop
        ) ;ret
       ) ;
    (check ret => 0)
    ;; "q" 被拦截并消费，不会误退出，直到遇到 escape
    (check q-handled-count => 2)
  ) ;let
) ;let

;; 7. 在 render 中输出到虚拟终端并验证屏幕内容
(let ((vt (make-virtual-terminal :width 40 :height 10))
      (st (make-pi-tui-state))
     ) ;
  (virtual-terminal-push-key! vt 'escape)
  (run-tui-loop vt
    :state st
    :render (lambda (s term)
              (terminal-set-cursor! term 0 0)
              (terminal-write term "PI-TUI-READY")
              (terminal-flush term)
            ) ;lambda
  ) ;run-tui-loop
  (let ((first-line (car (virtual-terminal-screen vt))))
    (check-true (string=? (substring first-line 0 12) "PI-TUI-READY"))
  ) ;let
) ;let

;; 8. 窗口 Resize 检测与 screen-resize! 自适应驱动
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ms (make-main-screen vt))
       (st (make-pi-tui-state :cols 80 :rows 24))
       (resize-detected? #f)
      ) ;
  ;; 模拟窗口大小由 80x24 调整为 100x30
  (virtual-terminal-resize! vt 100 30)
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt
                    :state st
                    :screen ms
                    :return-state #t
                    :render (lambda (s term)
                              (when (and (= (state-cols s) 100) (= (state-rows s) 30))
                                (set! resize-detected? #t)
                              ) ;when
                            ) ;lambda
                  ) ;run-tui-loop
        ) ;final-st
       ) ;
    (check (state-cols final-st) => 100)
    (check (state-rows final-st) => 30)
    (check resize-detected? => #t)
  ) ;let
) ;let

;; 9. 视口滚动按键（PageUp / PageDown）自动调整 scroll-offset
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (st (make-pi-tui-state :scroll-offset 0 :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt 'page-up)
  (virtual-terminal-push-key! vt 'page-up)
  (virtual-terminal-push-key! vt 'page-down)
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    (check (state-status final-st) => 'exit)
    ;; 两次 page-up (+5 +5 = 10)，一次 page-down (-5 = 5)
    (check (state-scroll-offset final-st) => 5)
  ) ;let
) ;let

(check-report)
