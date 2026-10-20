;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint runtime)
  (import (scheme base)
    (liii string)
    (liii pi blueprint types)
    (liii pi blueprint store)
  ) ;import
  (export pi-blueprint-current pi-blueprint-set-current!
    pi-blueprint-current-path pi-blueprint-set-current-path! pi-blueprint-load!
    pi-blueprint-save-current! pi-blueprint-append-event! pi-blueprint-clear!
    pi-blueprint-active? pi-blueprint-updated-with-history!
  ) ;export
  (begin

    ;; 当前蓝图状态 + 状态文件路径。脚本模式单进程单蓝图。
    (define %holder (vector #f #f))

    (define (pi-blueprint-current)
      (vector-ref %holder 0)
    ) ;define

    (define (pi-blueprint-set-current! bp)
      (vector-set! %holder 0 bp)
    ) ;define

    (define (pi-blueprint-current-path)
      (vector-ref %holder 1)
    ) ;define

    (define (pi-blueprint-set-current-path! path)
      (vector-set! %holder 1 path)
    ) ;define

    (define (pi-blueprint-active? bp)
      (and bp (string=? (pi-blueprint-status bp) "active"))
    ) ;define

    (define (pi-blueprint-load! state-path)
      (let ((bp (pi-blueprint-load-state state-path)))
        (pi-blueprint-set-current-path! state-path)
        (pi-blueprint-set-current! bp)
        bp
      ) ;let
    ) ;define

    (define (pi-blueprint-save-current!)
      (let ((bp (pi-blueprint-current)) (path (pi-blueprint-current-path)))
        (if (and bp path) (pi-blueprint-save-state! path bp) #f)
      ) ;let
    ) ;define

    (define (pi-blueprint-append-event! event task-id details)
      (let ((path (pi-blueprint-current-path)))
        (if path
          (pi-blueprint-append-history! path
            (pi-blueprint-history-entry event
              (pi-blueprint-active-phase-id (pi-blueprint-current))
              task-id
              details
            ) ;pi-blueprint-history-entry
          ) ;pi-blueprint-append-history!
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; 更新状态 → 落盘 → 记录审计流水，返回新的蓝图。
    (define (pi-blueprint-updated-with-history! bp event task-id details)
      (pi-blueprint-set-current! bp)
      (pi-blueprint-save-current!)
      (pi-blueprint-append-event! event task-id details)
      bp
    ) ;define

    (define (pi-blueprint-clear!)
      (pi-blueprint-set-current! #f)
      (pi-blueprint-set-current-path! #f)
    ) ;define

  ) ;begin
) ;define-library
