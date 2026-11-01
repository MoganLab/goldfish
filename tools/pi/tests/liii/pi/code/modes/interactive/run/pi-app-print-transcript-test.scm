(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive run)
) ;import

(check-set-mode! 'report)

;; 退出 TUI 后把对话记录打印回主屏（fullscreenExitOutput=transcript）。
;; 回归点：曾对消息列表调用 component-render，触发 type-error 并被静默 catch，
;; 结果退出全屏界面后终端里看不到任何会话内容。

;; 1. 标准终端：对话记录被写到输出端口，且包裹在 synchronized output 中批量输出
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf))
       (app (make-pi-app :terminal term :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-add-user-message! app "hello transcript")
  (pi-app-add-assistant-text! app "hi there")
  (pi-app-print-transcript! app)
  (let ((out (get-output-string buf)))
    (check-true (string-contains? out "hello transcript"))
    (check-true (string-contains? out "hi there"))
    ;; 对齐上游 TuiAltScreen.stop 行为：同步输出与 autowrap 控制保护
    (check-true (string-contains? out "\x1b;[?2026h"))
    (check-true (string-contains? out "\x1b;[?2026l"))
    (check-true (string-contains? out "\x1b;[?7l"))
    (check-true (string-contains? out "\x1b;[?7h"))
    (check-true (string-contains? out "\r\x1b;[2K"))
  ) ;let
) ;let*

;; 2. 没有消息时不输出任何内容
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf))
       (app (make-pi-app :terminal term :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-print-transcript! app)
  (check (get-output-string buf) => "")
) ;let*

(check-report)
