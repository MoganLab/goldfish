;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui loop)
  (import (scheme base)
    (liii error)
    (liii time)
    (liii tui terminal)
    (liii tui main-screen)
    (liii pi modes tui state)
    (liii pi modes tui editor)
    (liii pi modes tui layout)
    (liii pi modes tui loop opts)
  ) ;import
  (export run-tui-loop)
  (begin

    (define (run-tui-loop term . opts)
      (unless (terminal? term)
        (type-error "run-tui-loop: expected terminal")
      ) ;unless
      (let-values (((state max-ticks on-key on-tick render interval return-state? screen)
                    (%parse-loop-opts opts)
                   ) ;
                  ) ;
        (unless (pi-tui-state? state)
          (type-error "run-tui-loop: expected <pi-tui-state> for :state")
        ) ;unless
        (when (and screen (not (main-screen? screen)))
          (type-error "run-tui-loop: expected <main-screen> for :screen")
        ) ;when
        (when (and render (not (procedure? render)))
          (type-error "run-tui-loop: expected procedure for :render")
        ) ;when
        (when (and on-key (not (procedure? on-key)))
          (type-error "run-tui-loop: expected procedure for :on-key")
        ) ;when
        (when (and on-tick (not (procedure? on-tick)))
          (type-error "run-tui-loop: expected procedure for :on-tick")
        ) ;when

        (let loop
          ((ticks 0) (current-state state))
          (cond ((eq? (state-status current-state) 'exit) (if return-state? current-state 0))
                ((and max-ticks (>= ticks max-ticks)) (if return-state? current-state 0))
                (else
                  ;; 0. 终端 Resize 检测与自适应处理
                  (let-values (((term-cols term-rows) (detect-terminal-dimensions term)))
                    (when
                      (or (not (= term-cols (state-cols current-state)))
                        (not (= term-rows (state-rows current-state)))
                      ) ;or
                      (when screen
                        (handle-screen-resize! screen
                          (state-cols current-state)
                          (state-rows current-state)
                          term-cols
                          term-rows
                        ) ;handle-screen-resize!
                      ) ;when
                      (state-set-dimensions! current-state term-cols term-rows)
                      (state-mark-dirty! current-state)
                    ) ;when
                  ) ;let-values

                  ;; 1. 脏标记与渲染调度
                  (when (and render (state-dirty? current-state))
                    (state-clear-dirty! current-state)
                    (%call-render render current-state term)
                  ) ;when

                  ;; 2. 非阻塞按键轮询
                  (let* ((key (terminal-poll-key term)) (next-state current-state) (key-handled? #f))
                    (when key
                      (if on-key
                        (let ((res (%call-on-key on-key next-state key)))
                          (cond ((pi-tui-state? res) (set! next-state res))
                                ((eq? res 'exit) (state-set-status! next-state 'exit) (set! key-handled? #t))
                                ((eq? res 'handled) (set! key-handled? #t))
                                (else #f)
                          ) ;cond
                        ) ;let
                        #f
                      ) ;if
                      ;; 自动路由按键至处于 idle 状态的 pi-tui-editor
                      (when
                        (and (not key-handled?)
                          (not (eq? (state-status next-state) 'exit))
                          (eq? (state-status next-state) 'idle)
                          (pi-tui-editor? (state-input-editor next-state))
                        ) ;and
                        (let* ((ed (state-input-editor next-state))
                               (handled? (pi-tui-editor-handle-key ed key))
                              ) ;
                          (when handled?
                            (set! key-handled? #t)
                            (state-mark-dirty! next-state)
                          ) ;when
                        ) ;let*
                      ) ;when
                      ;; 终止按键判定：
                      ;; 1) escape 键直接触发退出
                      ;; 2) 未被拦截的 "q" / "Q" 触发退出
                      (cond ((eq? key 'escape) (state-set-status! next-state 'exit))
                            ((and (not key-handled?) (or (equal? key "q") (equal? key "Q")))
                             (state-set-status! next-state 'exit)
                            ) ;
                      ) ;cond
                    ) ;when

                    ;; 3. 检查按键后是否要求退出
                    (if (eq? (state-status next-state) 'exit)
                      (if return-state? next-state 0)
                      ;; 4. Tick 回调调度
                      (let ((after-tick-state next-state))
                        (when on-tick
                          (let ((res (%call-on-tick on-tick after-tick-state ticks)))
                            (cond ((pi-tui-state? res) (set! after-tick-state res))
                                  ((eq? res 'exit) (state-set-status! after-tick-state 'exit))
                                  (else #f)
                            ) ;cond
                          ) ;let
                        ) ;when

                        ;; 5. 检查 tick 后是否要求退出
                        (if (eq? (state-status after-tick-state) 'exit)
                          (if return-state? after-tick-state 0)
                          ;; 6. 节拍控制并推进到下一轮
                          (begin
                            (when (standard-terminal? term)
                              (sleep interval)
                            ) ;when
                            (loop (+ ticks 1) after-tick-state)
                          ) ;begin
                        ) ;if
                      ) ;let
                    ) ;if
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
