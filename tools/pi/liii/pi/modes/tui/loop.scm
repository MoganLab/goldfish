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
    (liii pi modes tui state)
  ) ;import
  (export run-tui-loop)
  (begin

    (define (%call-render render-proc state term)
      (let* ((ar (arity render-proc)) (min-args (car ar)))
        (cond ((>= min-args 2) (render-proc state term))
              ((= min-args 1) (render-proc state))
              (else (render-proc))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%call-on-key on-key-proc state key)
      (let* ((ar (arity on-key-proc)) (min-args (car ar)))
        (if (<= min-args 1) (on-key-proc key) (on-key-proc state key))
      ) ;let*
    ) ;define

    (define (%call-on-tick on-tick-proc state ticks)
      (let* ((ar (arity on-tick-proc)) (min-args (car ar)))
        (cond ((>= min-args 2) (on-tick-proc state ticks))
              ((= min-args 1) (on-tick-proc state))
              (else (on-tick-proc))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%parse-loop-opts opts)
      (let loop
        ((rest opts)
         (state #f)
         (max-ticks #f)
         (on-key #f)
         (on-tick #f)
         (render #f)
         (interval 0.033)
         (return-state? #f)
        ) ;
        (cond
         ((null? rest)
          (values (or state (make-pi-tui-state))
            max-ticks
            on-key
            on-tick
            render
            interval
            return-state?
          ) ;values
         ) ;
         ((eq? (car rest) :state)
          (loop (cddr rest)
            (cadr rest)
            max-ticks
            on-key
            on-tick
            render
            interval
            return-state?
          ) ;loop
         ) ;
         ((eq? (car rest) :max-ticks)
          (loop (cddr rest)
            state
            (cadr rest)
            on-key
            on-tick
            render
            interval
            return-state?
          ) ;loop
         ) ;
         ((eq? (car rest) :on-key)
          (loop (cddr rest)
            state
            max-ticks
            (cadr rest)
            on-tick
            render
            interval
            return-state?
          ) ;loop
         ) ;
         ((eq? (car rest) :on-tick)
          (loop (cddr rest)
            state
            max-ticks
            on-key
            (cadr rest)
            render
            interval
            return-state?
          ) ;loop
         ) ;
         ((eq? (car rest) :render)
          (loop (cddr rest)
            state
            max-ticks
            on-key
            on-tick
            (cadr rest)
            interval
            return-state?
          ) ;loop
         ) ;
         ((eq? (car rest) :interval)
          (loop (cddr rest)
            state
            max-ticks
            on-key
            on-tick
            render
            (cadr rest)
            return-state?
          ) ;loop
         ) ;
         ((eq? (car rest) :return-state)
          (loop (cddr rest) state max-ticks on-key on-tick render interval (cadr rest))
         ) ;
         ((exact-integer? (car rest))
          (loop (cdr rest) state (car rest) on-key on-tick render interval return-state?)
         ) ;
         (else (loop (cdr rest) state max-ticks on-key on-tick render interval return-state?)
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-tui-loop term . opts)
      (unless (terminal? term)
        (type-error "run-tui-loop: expected terminal")
      ) ;unless
      (let-values (((state max-ticks on-key on-tick render interval return-state?)
                    (%parse-loop-opts opts)
                   ) ;
                  ) ;
        (unless (pi-tui-state? state)
          (type-error "run-tui-loop: expected <pi-tui-state> for :state")
        ) ;unless
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
