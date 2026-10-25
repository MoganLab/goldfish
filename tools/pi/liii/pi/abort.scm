;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi abort)
  ;; 会话级中止信号注册点（参考上游 pi 的 AbortController/signal 语义）：
  ;; TUI 主线程按 ESC 后向 abort-chan 发送 'abort，worker 会话在
  ;; 流式回调 / turn 边界 / 工具边界等检查点调用 pi-aborted? 探测。
  ;; 信号经 chan 跨线程传递，检查逻辑与一次性标志缓存在本会话内，
  ;; 与 set-pi-retry-logger! 同属会话级注入模式，不污染其他会话。
  (export pi-abort-arm-chan! pi-abort-reset! pi-aborted?)
  (import (scheme base) (liii go))
  (begin

    ;; 当前 arm 的 abort 信号通道；#f 表示未 arm（安全默认：永不中止）
    (define *abort-chan* #f)

    ;; 一次性中止标志：首次探测到信号后置位，后续检查点直接命中缓存，
    ;; 保证信号被消费一次后所有检查点仍能一致观察到中止状态
    (define *aborted?* #f)

    ;; arm 到新的 abort 信号通道：排空上一轮任务的残留信号并复位标志。
    ;; worker 每次从 jobs-chan 取出新任务后调用，防止旧 abort 误伤新任务
    (define (pi-abort-arm-chan! ch)
      ;; 排空 chan 中残留信号（容量有限，循环取尽）
      (let drain-loop
        ()
        (when (chan-try-recv! ch #f)
          (drain-loop)
        ) ;when
      ) ;let
      (set! *abort-chan* ch)
      (set! *aborted?* #f)
    ) ;define

    ;; 复位为未 arm 状态（测试与无中止场景的安全默认）
    (define (pi-abort-reset!)
      (set! *abort-chan* #f)
      (set! *aborted?* #f)
    ) ;define

    ;; 探测中止状态：非阻塞拉取 chan 中的 abort 信号；
    ;; 一旦取到信号即置位缓存，之后所有调用恒返回 #t
    (define (pi-aborted?)
      (when (and (not *aborted?*) *abort-chan*)
        (when (chan-try-recv! *abort-chan* #f)
          (set! *aborted?* #t)
        ) ;when
      ) ;when
      *aborted?*
    ) ;define

  ) ;begin
) ;define-library
