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

;; 1. 标准终端：对话记录被写到输出端口，且离开备用屏移入 synchronized output 中原子刷新
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
    ;; 子任务 1：同步输出协议严格包裹切出备用屏序列，且 \\x1b;[?2026h 先于 \\x1b;[?1049l
    (let ((idx-sync (string-position "\x1b;[?2026h" out))
          (idx-exit (string-position "\x1b;[?1049l" out))
         ) ;
      (check-true (and idx-sync idx-exit (< idx-sync idx-exit)))
    ) ;let
    (check-true (string-contains? out "\x1b;[?2026l"))
    (check-true (string-contains? out "\x1b;[?7l"))
    (check-true (string-contains? out "\x1b;[?7h"))
    (check-true (string-contains? out "\x1b;[?25h"))
    (check-true (string-contains? out "\x1b;[0m"))
    ;; 子任务 2：去除冗余的清行前缀，纯净换行流
    (check-false (string-contains? out "\r\x1b;[2K"))
    ;; 子任务 3：整块原子输出首尾控制序列
    (check-true (string-starts? out "\x1b;[?2026h\x1b;[?1049l\x1b;[?7l"))
    (check-true (string-ends? out "\x1b;[0m\x1b;[?7h\r\n\x1b;[?25h\x1b;[?2026l"))
  ) ;let
) ;let*

;; 2. 空历史会话场景：仅输出切回主屏与复位序列，不输出多余换行
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf))
       (app (make-pi-app :terminal term :handler (lambda (a msg) #t)))
      ) ;
  (pi-app-print-transcript! app)
  (let ((out (get-output-string buf)))
    (check out => "\x1b;[?2026h\x1b;[?1049l\x1b;[0m\x1b;[?7h\x1b;[?25h\x1b;[?2026l")
    (check-false (string-contains? out "\r\n"))
    (check-false (string-contains? out "\x1b;[?7l"))
  ) ;let
) ;let*

(check-report)
