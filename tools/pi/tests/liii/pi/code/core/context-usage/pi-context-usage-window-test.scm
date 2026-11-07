;; pi-context-usage-window
;; 读取上下文占用 record 中的上下文窗口容量。
;;
;; 语法
;; ----
;; (pi-context-usage-window ctx)
;;
;; 参数
;; ----
;; ctx : pi-context-usage
;; 上下文占用 record。
;;
;; 返回值
;; ----
;; number | #f
;; 上下文窗口容量（Token 数）；未知时为 #f。

(import (scheme base) (liii check) (liii pi code core context-usage))

(check-set-mode! 'report-failed)

;; 1. 已知窗口：原样返回容量
(check (pi-context-usage-window (make-pi-context-usage 12.3 1048576 #t))
  =>
  1048576
) ;check

;; 2. 占比未知时窗口容量仍保留（页脚显示 "?/1.0M"）
(check (pi-context-usage-window (make-pi-context-usage #f 1048576 #f))
  =>
  1048576
) ;check

;; 3. 窗口缺失时返回 #f
(check (pi-context-usage-window (make-pi-context-usage 12.3 #f #t)) => #f)

(check-report)
