(import (liii check) (liii tui terminal) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; 1. 测试 virtual-terminal 初始无输入时返回 #f
(let ((vt (make-virtual-terminal 40 20)))
  (check (terminal-poll-key vt) => #f)
) ;let

;; 2. 测试 virtual-terminal 注入按键后顺序弹出
(let ((vt (make-virtual-terminal 40 20)))
  (virtual-terminal-push-key! vt 'up)
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt "w")
  (virtual-terminal-push-key! vt 'enter)

  (check (terminal-poll-key vt) => 'up)
  (check (terminal-poll-key vt) => 'down)
  (check (terminal-poll-key vt) => "w")
  (check (terminal-poll-key vt) => 'enter)
  (check (terminal-poll-key vt) => #f)
) ;let

;; 3. 测试非法参数类型错误
(check-catch 'type-error (terminal-poll-key 123))
(check-catch 'type-error (terminal-poll-key "invalid"))
(check-catch 'type-error (virtual-terminal-push-key! "invalid" 'up))
(check-catch 'type-error (virtual-terminal-pop-key! "invalid"))

;; 4. 测试无参数调用（当前无按键时返回 #f 或非阻塞）
(check (boolean? (terminal-poll-key)) => #t)

;; 5. 测试 ANSI 转义字节流解析为符号
(check (%resolve-ansi-bytes '(27)) => 'escape)
;; CSI 序列 (ESC [ ...)
(check (%resolve-ansi-bytes '(27 91 65)) => 'up)
(check (%resolve-ansi-bytes '(27 91 66)) => 'down)
(check (%resolve-ansi-bytes '(27 91 67)) => 'right)
(check (%resolve-ansi-bytes '(27 91 68)) => 'left)
(check (%resolve-ansi-bytes '(27 91 72)) => 'home)
(check (%resolve-ansi-bytes '(27 91 70)) => 'end)
(check (%resolve-ansi-bytes '(27 91 51 126)) => 'delete)
(check (%resolve-ansi-bytes '(27 91 53 126)) => 'page-up)
(check (%resolve-ansi-bytes '(27 91 54 126)) => 'page-down)
;; SS3 序列 (ESC O ...)
(check (%resolve-ansi-bytes '(27 79 65)) => 'up)
(check (%resolve-ansi-bytes '(27 79 66)) => 'down)
(check (%resolve-ansi-bytes '(27 79 67)) => 'right)
(check (%resolve-ansi-bytes '(27 79 68)) => 'left)
(check (%resolve-ansi-bytes '(27 79 72)) => 'home)
(check (%resolve-ansi-bytes '(27 79 70)) => 'end)
(check (%resolve-ansi-bytes '(27 79 80)) => 'f1)
(check (%resolve-ansi-bytes '(27 79 81)) => 'f2)

;; 6. 测试 UTF-8 多字节解析
(check (%resolve-utf8-bytes '(228 189 160)) => "你")
(check (%resolve-utf8-bytes '(229 165 189)) => "好")
(check (%resolve-utf8-bytes '(240 159 154 128)) => "🚀")
(check (%resolve-utf8-bytes '()) => #f)
(check (%resolve-utf8-bytes '(255 255)) => #f)

;; 6. 测试 Windows 控制台扩展键码映射 (特别是 Shift-Tab 键码 15 与 F1~F12 功能键)
(check (%map-extended-key 72) => 'up)
(check (%map-extended-key 80) => 'down)
(check (%map-extended-key 75) => 'left)
(check (%map-extended-key 77) => 'right)
(check (%map-extended-key 15) => 'shift-tab)
(check (%map-extended-key 59) => 'f1)
(check (%map-extended-key 60) => 'f2)
(check (%map-extended-key 68) => 'f10)
(check (%map-extended-key 133) => 'f11)
(check (%map-extended-key 134) => 'f12)
(check (%map-extended-key 999) => #f)

(check-report)
