;; console-getch
;; 从控制台读取一个按键字节（不经行缓冲与回显）。
;;
;; 语法
;; ----
;; (console-getch)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; exact-integer? 或 #f
;; 返回读到的字节值；无输入或非控制台环境时返回 #f。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 在无输入或非控制台时安全返回（整数或 #f）
(let ((c (console-getch)))
  (check-true (or (boolean? c) (exact-integer? c)))
) ;let

(check-report)
