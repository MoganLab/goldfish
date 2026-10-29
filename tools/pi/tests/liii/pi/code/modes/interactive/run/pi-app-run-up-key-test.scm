(import (scheme base)
  (liii list)
  (liii string)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report)

;; 复现：↑ 键（向上）在 ESC 中断之后不再回溯输入历史。
;;
;; 上游 pi 的语义（packages/tui/src/components/editor.ts）：
;;   cursorUp 落在首个可视行时，若编辑器为空 / 正在浏览历史 / 光标在行首，
;;   则 navigateHistory(-1)（回溯上一条 prompt）；否则移动光标。
;; 本工程的编辑器此前在 multiline 模式下无条件走 buffer-move-up!，
;; 因此 pi（:multiline #t）里 ↑ 永远无法回溯历史。

;; 1. 基线：提交一条消息后按 ↑ 回溯出上一条 prompt，再次回车即重新提交
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (received '())
       (app
         (make-pi-app
           :terminal vt
           :handler  (lambda (a msg) (set! received (cons msg received)))
         ) ;make-pi-app
       ) ;app
      ) ;
  (virtual-terminal-push-key! vt "hello")
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; 第二次提交的内容与第一次相同：说明 ↑ 确实回溯出了上一条 prompt
  (check (reverse received) => '("hello" "hello"))
) ;let*

;; 2. ESC 中断（busy 时发 abort 信号并撤回排队消息）后按 ↑，
;;    同样应能回溯出上一条 prompt
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a msg) #t)))
       (q (pi-app-queue app))
      ) ;
  ;; 构造 busy 状态：ESC 中断会发 abort 信号并撤回排队消息到编辑器
  (pi-queue-enqueue! q "queued-1")
  (pi-queue-set-busy! q)
  (virtual-terminal-push-key! vt "hello")
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'escape)
  ;; ESC 撤回排队消息后编辑器内容为 "queued-1\nhello"、光标在行首：
  ;; 此时 ↑ 直接回溯输入历史（编辑器非空但光标位于行首）
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'ctrl-d)
  (check (pi-app-run app) => #t)
  ;; abort 信号已发送
  (check (chan-try-recv! (pi-app-abort-chan app) #f) => 'abort)
  ;; ↑ 回溯出上一条 prompt（"hello"）并重新提交：
  ;; 若 ↑ 失效，提交的将是撤回的排队消息 "queued-1\nhello"
  (check (pi-queue-items q) => '("hello"))
) ;let*

(check-report)
