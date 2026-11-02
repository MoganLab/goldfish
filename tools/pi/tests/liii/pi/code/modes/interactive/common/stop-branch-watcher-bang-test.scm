;; stop-branch-watcher!
;; 停止 Git 分支文件变动监听器。
;;
;; 语法
;; ----
;; (stop-branch-watcher! obs)
;;
;; 参数
;; ----
;; obs : any
;; make-branch-watcher 返回的 observer 实例，容忍 #f 或非 observer 对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若成功执行停止返回 #t，若传入 #f 或非 observer 则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive common))

(check-set-mode! 'report-failed)

;; 1. 容忍 #f，返回 #f
(check-false (stop-branch-watcher! #f))

;; 2. 容忍非 observer 对象，返回 #f
(check-false (stop-branch-watcher! "invalid-obs"))
(check-false (stop-branch-watcher! 123))

(check-report)
