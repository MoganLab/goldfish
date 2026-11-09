;; %read-escape-bytes
;; 从控制台读取一个完整的转义序列字节帧。
;;
;; 语法
;; ----
;; (%read-escape-bytes)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list?
;; 按到达顺序排列的字节列表，首字节固定为 ESC(27)。
;;
;; 说明
;; ----
;; 逐字节累积，帧完整即返回，不额外等待超时。ESC CR 之后若在 10ms 窗口内
;; 到达 LF，则并成 ESC CR LF 一帧返回（部分终端的 Alt+Enter 形态）；
;; 窗口内到达的其他字节回填到内部队列，不丢键。

(import (scheme base) (liii check) (liii tui terminal escape))

(check-set-mode! 'report-failed)

;; 非交互场景（stdin 非 tty 或此刻没有输入）下不阻塞、不报错：
;; 只返回已经读到的 ESC 首字节
(check (%read-escape-bytes) => (list 27))

(check-report)
