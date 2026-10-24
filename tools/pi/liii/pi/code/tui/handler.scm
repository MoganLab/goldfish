;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tui handler)
  (export make-tui-agent-handler)
  (import (scheme base)
    (liii pi code config)
    (liii pi agent queue)
    (liii pi code tui app)
    (liii pi code tui worker)
    (liii pi code modes flow)
  ) ;import
  (begin

    ;; 测试接缝：构造同步执行的 agent handler，与生产路径共用同一套
    ;; 事件翻译（make-tui-emit），runner 可注入 mock 以避免真实后台线程
    (define* (make-tui-agent-handler parsed cfg (runner run-session-flow))
      (lambda (app user-text)
        (let ((cred-msg (pi-missing-credential-msg cfg)))
          (if cred-msg
            (begin
              (pi-app-add-tool-result! app "auth" '() 'error cred-msg)
              (pi-queue-finish-task! (pi-app-queue app))
              (pi-app-drain-queue! app)
            ) ;begin
            (let ((ch (pi-app-event-chan app)))
              (runner parsed cfg user-text (make-tui-emit ch) (lambda (res-msgs) #t))
              (pi-app-poll-events! app)
            ) ;let
          ) ;if
        ) ;let
      ) ;lambda
    ) ;define*

  ) ;begin
) ;define-library
