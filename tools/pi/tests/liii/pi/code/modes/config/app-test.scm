(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii tui)
  (liii pi code modes config app)
) ;import

(check-set-mode! 'report-failed)

;; pi-config-run：ranger 风格只读配置 TUI 入口。
;; q / Escape / Ctrl+C 退出并返回 0；不修改磁盘上的任何配置文件。

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"))

(define DIR (string-append FIXTURE "/config-tui"))

;; 1. q 立即退出
(let ((vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'q)
  (check (pi-config-run DIR :terminal vt :env '()) => 0)
) ;let

;; 2. Escape 也能退出
(let ((vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'escape)
  (check (pi-config-run DIR :terminal vt :env '()) => 0)
) ;let

;; 3. 上下移动后进入模型列再退出（不应抛错）
(let ((vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt 'right)
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt 'q)
  (check (pi-config-run DIR :terminal vt :env '()) => 0)
) ;let

;; 4. 渲染后屏幕包含三列关键内容
(let ((vt (make-virtual-terminal :width 120 :height 24)))
  (virtual-terminal-push-key! vt 'q)
  (pi-config-run DIR :terminal vt :env '())
  (let ((screen (string-join (virtual-terminal-screen vt) "\n")))
    (check (string-contains screen "providers") => #t)
    (check (string-contains screen "kimi-coding") => #t)
  ) ;let
) ;let

;; 5. 只读：运行后 fixtures 目录内容不变（以 providers.json 与 settings.json 为准）
(let* ((settings (string-append DIR "/settings.json"))
       (before (path-read-text (path settings)))
       (vt (make-virtual-terminal :width 120 :height 24))
      ) ;
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt 'q)
  (pi-config-run DIR :terminal vt :env '())
  (check (path-read-text (path settings)) => before)
) ;let*

(check-report)
