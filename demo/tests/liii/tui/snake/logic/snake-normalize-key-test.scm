;; snake-normalize-key
;; 把按键输入归一化为方向/暂停/退出命令符号
;;
;; 语法
;; ----
;; (snake-normalize-key k)
;;
;; 参数
;; ----
;; k : symbol 或 string\n原始按键输入
;;
;; 返回值
;; ------
;; symbol? 或 #f\nup/down/left/right/pause/quit，无法识别时返回 #f

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; 1. 方向键符号原样通过
(check (snake-normalize-key 'up) => 'up)
(check (snake-normalize-key 'down) => 'down)
(check (snake-normalize-key 'left) => 'left)
(check (snake-normalize-key 'right) => 'right)

;; 2. WASD 大小写映射
(check (snake-normalize-key "w") => 'up)
(check (snake-normalize-key "W") => 'up)
(check (snake-normalize-key "s") => 'down)
(check (snake-normalize-key "a") => 'left)
(check (snake-normalize-key "D") => 'right)

;; 3. 暂停键映射
(check (snake-normalize-key 'space) => 'pause)
(check (snake-normalize-key " ") => 'pause)
(check (snake-normalize-key "p") => 'pause)
(check (snake-normalize-key "P") => 'pause)

;; 4. 退出键映射
(check (snake-normalize-key 'escape) => 'quit)
(check (snake-normalize-key 'ctrl-c) => 'quit)
(check (snake-normalize-key "q") => 'quit)
(check (snake-normalize-key "Q") => 'quit)

;; 5. 未识别输入返回 #f
(check (snake-normalize-key 'unknown) => #f)
(check (snake-normalize-key "x") => #f)

(check-report)
