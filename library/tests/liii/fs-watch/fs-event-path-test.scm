;; fs-event-path
;; 读取文件系统事件记录的目标路径。
;;
;; 语法
;; ----
;; (fs-event-path event)
;;
;; 参数
;; ----
;; event : fs-event?
;; 事件记录。
;;
;; 返回值
;; ----
;; string
;; 事件目标路径。
;;
;; 说明
;; ----
;; macOS kqueue 后端只产生目录级 'modified 事件，此时 path 为监听目录本身。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(check (fs-event-path (make-fs-event 'modified "/x/HEAD" #f)) => "/x/HEAD")
(check (fs-event-path (make-fs-event 'deleted "/a/b" #f)) => "/a/b")
(check (fs-event-path (make-fs-event 'moved "/a/old" "/a/new")) => "/a/old")

(check-report)
