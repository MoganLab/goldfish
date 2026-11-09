;; %escape-bytes-complete?
;; 判断已累积的转义序列字节是否构成一个完整帧。
;;
;; 语法
;; ----
;; (%escape-bytes-complete? bytes)
;;
;; 参数
;; ----
;; bytes : list?
;; 按到达顺序排列的字节（首字节固定为 ESC 27）。
;;
;; 返回值
;; ----
;; boolean
;; 帧完整返回 #t，还需继续读取返回 #f。
;;
;; 说明
;; ----
;; ESC [ ... <终结字节> 与 ESC O <终结字节> 需要读到 0x40~0x7E 的终结字节
;; 才算完整；ESC <字符> 两字节即成帧。帧完整即停止读取，避免每个转义序列
;; 都在末尾多等一次超时（鼠标滚轮事件因此凭空多出 10ms 延迟）。

(import (scheme base) (liii check) (liii tui terminal escape))

(check-set-mode! 'report-failed)

;; ESC 单独一个字节：不足成帧
(check (%escape-bytes-complete? (list 27)) => #f)

;; ESC <字符>：两字节即成帧
(check (%escape-bytes-complete? (list 27 13)) => #t)
(check (%escape-bytes-complete? (list 27 10)) => #t)
(check (%escape-bytes-complete? (list 27 97)) => #t)

;; ESC O（SS3）：需终结字节
(check (%escape-bytes-complete? (list 27 79)) => #f)
(check (%escape-bytes-complete? (list 27 79 77)) => #t)

;; ESC [（CSI）：未读到终结字节前不完整
(check (%escape-bytes-complete? (list 27 91)) => #f)
(check (%escape-bytes-complete? (list 27 91 49 59)) => #f)
(check (%escape-bytes-complete? (list 27 91 65)) => #t)

;; SGR 鼠标序列（ESC [ < ... M/m）
(check (%escape-bytes-complete? '(27 91 60 48 59 49 48 59 53)) => #f)
(check (%escape-bytes-complete? '(27 91 60 48 59 49 48 59 53 77)) => #t)

(check-report)
