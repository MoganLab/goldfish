;; terminal-poll-key
;; 从终端读取一个按键或鼠标事件（非阻塞）。
;;
;; 语法
;; ----
;; (terminal-poll-key [term])
;;
;; 参数
;; ----
;; term : terminal? (可选)
;; 目标终端，缺省为当前标准终端。
;;
;; 返回值
;; ----
;; any 或 #f
;; 读取到的按键符号、字符串或 mouse-event 对象；当前无输入时返回 #f。

(import (scheme base) (liii check) (liii tui terminal) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; 1. virtual-terminal 初始无输入时返回 #f
(let ((vt (make-virtual-terminal 40 20)))
  (check (terminal-poll-key vt) => #f)
) ;let

;; 2. virtual-terminal 注入按键后顺序弹出
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

;; 3. 非法参数类型错误
(check-catch 'type-error (terminal-poll-key 123))
(check-catch 'type-error (terminal-poll-key "invalid"))

;; 4. 无参数调用（当前无按键时返回 #f 或非阻塞）
(check (boolean? (terminal-poll-key)) => #t)

(check-report)
