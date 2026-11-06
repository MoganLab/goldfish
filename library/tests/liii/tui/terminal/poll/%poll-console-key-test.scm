;; %poll-console-key
;; 从当前控制台非阻塞地读取一个按键。
;;
;; 语法
;; ----
;; (%poll-console-key)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; any 或 #f
;; 读取到的按键符号、字符串或 mouse-event 对象；当前无输入时返回 #f。

(import (scheme base) (liii check) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; 无输入时返回 #f（非阻塞）
(check (%poll-console-key) => #f)

(check-report)
