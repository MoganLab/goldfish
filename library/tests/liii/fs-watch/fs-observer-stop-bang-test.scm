;; fs-observer-stop!
;; 停止监听：逐个停掉 C 层 watcher（后台泵线程收到 'stopped 后自行退出）。
;;
;; 语法
;; ----
;; (fs-observer-stop! obs)
;;
;; 参数
;; ----
;; obs : fs-observer?
;; observer 对象。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 幂等，可重复调用。
;; 2. 未 start 直接 stop 无害。
;; 3. stop 后允许重新 schedule + start（复用同一 observer）。
;;
;; 错误处理
;; ----
;; obs 非 observer 时抛出 type-error。

(import (scheme base) (liii check) (liii tempfile) (liii fs-watch) (liii shutil))

(check-set-mode! 'report-failed)

;; 未 start 直接 stop：无害且幂等
(let ((obs (make-fs-observer)))
  (fs-observer-stop! obs)
  (fs-observer-stop! obs)
) ;let

;; 参数校验
(check-catch 'type-error (fs-observer-stop! "x"))

(check-report)
