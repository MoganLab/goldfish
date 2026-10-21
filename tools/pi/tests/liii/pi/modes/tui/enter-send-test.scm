(import (liii check)
  (liii error)
  (liii tui terminal)
  (liii tui main-screen)
  (liii pi modes tui)
  (liii pi modes tui state)
  (liii pi modes tui loop)
  (liii pi modes tui editor)
) ;import

(check-set-mode! 'report-failed)

;; 1. 当按键为 enter 时，输入框非空触发提交与状态流转
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "h")
  (virtual-terminal-push-key! vt "i")
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    (check (pi-tui-state? final-st) => #t)
    ;; 验证 messages 里包含 user 消息 "hi"
    (check (state-messages final-st) => '((user . "hi")))
    ;; 验证输入框被清空
    (check (pi-tui-editor-text ed) => "")
  ) ;let
) ;let*

;; 2. 当输入框为空时，按 enter 不应产生空消息或切换状态
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt 'enter)
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    (check (state-messages final-st) => '())
  ) ;let
) ;let*

;; 3. Shift+Enter 折行不回归
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (ed (make-pi-tui-editor :focused? #t))
       (st (make-pi-tui-state :input-editor ed :status 'idle))
      ) ;
  (virtual-terminal-push-key! vt "line1")
  (virtual-terminal-push-key! vt 'shift-enter)
  (virtual-terminal-push-key! vt "line2")
  (virtual-terminal-push-key! vt 'escape)

  (let ((final-st (run-tui-loop vt :state st :return-state #t)))
    ;; 尚未按普通 enter，消息列表为空
    (check (state-messages final-st) => '())
    ;; 输入框内容包含换行
    (check (pi-tui-editor-text ed) => "line1\nline2")
  ) ;let
) ;let*

(check-report)
