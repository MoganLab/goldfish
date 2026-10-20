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
    (liii pi modes tui loop dispatch)
    (liii pi modes tui streaming)
  ) ;import
  (export run-tui-loop)
  (begin

    (define (%handle-resize! term screen current-state)
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
    ) ;define

    (define (%dispatch-tick on-tick state ticks)
      (if (not on-tick)
        state
        (let ((res (%call-on-tick on-tick state ticks)))
          (cond ((pi-tui-state? res) res)
                ((eq? res 'exit) (state-set-status! state 'exit) state)
                (else state)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

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
                  (%handle-resize! term screen current-state)

                  ;; 1. 脏标记与渲染调度
                  (when (state-dirty? current-state)
                    (state-clear-dirty! current-state)
                    (cond (render (%call-render render current-state term))
                          (screen (render-tui-screen! screen current-state))
                    ) ;cond
                  ) ;when

                  ;; 2. 非阻塞按键轮询与分发
                  (let* ((key (terminal-poll-key term))
                         (next-state (if key (%dispatch-tui-key current-state key on-key) current-state))
                        ) ;
                    ;; 3. 检查按键后是否要求退出
                    (if (eq? (state-status next-state) 'exit)
                      (if return-state? next-state 0)
                      ;; 4. Tick 回调调度
                      (let ((after-tick-state (%dispatch-tick on-tick next-state ticks)))
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
