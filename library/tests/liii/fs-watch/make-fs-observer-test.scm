;; make-fs-observer
;; 创建文件系统监听 observer（对齐 Python watchdog 的 Observer）。
;;
;; 语法
;; ----
;; (make-fs-observer)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; fs-observer?
;; 新的 observer 对象；初始无监听项、未运行。
;;
;; 说明
;; ----
;; 生命周期：make-fs-observer → fs-observer-schedule!（可多次）→ fs-observer-start!
;; → ... → fs-observer-stop!（幂等）→ 可再次 schedule + start 复用。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(let ((obs (make-fs-observer)))
  (check-true (fs-observer? obs))
) ;let

(check-report)
