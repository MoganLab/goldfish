;; console-kbhit?
;; 非阻塞检测控制台输入缓冲区是否已有按键可读。
;;
;; 语法
;; ----
;; (console-kbhit? [timeout-ms])
;;
;; 参数
;; ----
;; timeout-ms : exact-integer?（可选）
;; 等待输入的超时毫秒数，缺省为 0（立即返回）。
;;
;; 返回值
;; -----
;; boolean?
;; 输入缓冲区有数据返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;; 当 timeout-ms 不是整数时抛出错误。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; 支持无参和可选毫秒超时参数，返回布尔值
(check-true (boolean? (console-kbhit?)))
(check-true (boolean? (console-kbhit? 0)))
(check-true (boolean? (console-kbhit? 10)))

;; 参数类型错误
(check-catch 'type-error (console-kbhit? "invalid"))
(check-catch 'type-error (console-kbhit? 1.5))

(check-report)
