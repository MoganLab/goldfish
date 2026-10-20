;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui loop opts)
  (import (scheme base) (liii error) (liii pi modes tui state))
  (export %parse-loop-opts %call-render %call-on-key %call-on-tick)
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
         (screen #f)
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
            screen
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
            screen
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
            screen
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
            screen
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
            screen
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
            screen
          ) ;loop
         ) ;
         ((eq? (car rest) :screen)
          (loop (cddr rest)
            state
            max-ticks
            on-key
            on-tick
            render
            interval
            return-state?
            (cadr rest)
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
            screen
          ) ;loop
         ) ;
         ((eq? (car rest) :return-state)
          (loop (cddr rest)
            state
            max-ticks
            on-key
            on-tick
            render
            interval
            (cadr rest)
            screen
          ) ;loop
         ) ;
         ((exact-integer? (car rest))
          (loop (cdr rest)
            state
            (car rest)
            on-key
            on-tick
            render
            interval
            return-state?
            screen
          ) ;loop
         ) ;
         (else (loop (cdr rest)
                 state
                 max-ticks
                 on-key
                 on-tick
                 render
                 interval
                 return-state?
                 screen
               ) ;loop
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
