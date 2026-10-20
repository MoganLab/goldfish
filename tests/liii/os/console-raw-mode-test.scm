(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 1. 参数类型检查
(check-catch 'type-error (console-raw-mode! "invalid"))
(check-catch 'type-error (console-raw-mode! 123))
(check-catch 'type-error (console-raw-mode! 'symbol))
(check-catch 'type-error (console-raw-mode! '()))

;; 2. console-raw-mode! 调用返回布尔值
(check-true (boolean? (console-raw-mode! #f)))

;; 3. console-kbhit? 返回布尔值（支持无参和可选毫秒超时参数）
(check-true (boolean? (console-kbhit?)))
(check-true (boolean? (console-kbhit? 0)))
(check-true (boolean? (console-kbhit? 10)))
(check-catch 'type-error (console-kbhit? "invalid"))
(check-catch 'type-error (console-kbhit? 1.5))

;; 4. console-getch 在无输入或非控制台时安全返回（整数或 #f）
(let ((c (console-getch)))
  (check-true (or (boolean? c) (exact-integer? c)))
) ;let

(check-report)
