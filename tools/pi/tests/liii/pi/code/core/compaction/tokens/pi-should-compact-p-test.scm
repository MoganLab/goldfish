;; pi-should-compact?
;; 判断当前上下文 token 数是否达到压缩阈值。
;;
;; 语法
;; ----
;; (pi-should-compact? context-tokens context-window settings)
;;
;; 参数
;; ----
;; context-tokens : number
;; 当前上下文已占用的 token 数。
;;
;; context-window : number
;; 模型上下文窗口大小。
;;
;; settings : pi-compaction-settings
;; 压缩配置记录。
;;
;; 返回值
;; ----
;; boolean
;; 达到压缩阈值时返回 #t，否则返回 #f。

(import (liii check) (liii pi code core compaction))

(check-set-mode! 'report-failed)

;; contextWindow - reserveTokens 为阈值
(let ((s (make-pi-compaction-settings #t 20 20000)))
  (check (pi-should-compact? 81 100 s) => #t)
  (check (pi-should-compact? 80 100 s) => #f)
) ;let

;; enabled=false 不触发
(let ((s (make-pi-compaction-settings #f 20 20000)))
  (check (pi-should-compact? 1000 100 s) => #f)
) ;let

;; 默认设置使用默认阈值
(let ((s (pi-compaction-default-settings)))
  (check (pi-should-compact? 16383 100000 s) => #f)
  (check (pi-should-compact? 90000 100000 s) => #t))

(check-report)
