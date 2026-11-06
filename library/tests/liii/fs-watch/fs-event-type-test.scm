;; fs-event-type
;; 读取文件系统事件记录的事件类型。
;;
;; 语法
;; ----
;; (fs-event-type event)
;;
;; 参数
;; ----
;; event : fs-event?
;; 事件记录。
;;
;; 返回值
;; ----
;; symbol
;; 事件类型：'modified / 'created / 'deleted / 'moved。
;;
;; 说明
;; ----
;; macOS kqueue 后端只产生目录级 'modified 事件；
;; Windows/Linux 提供具体文件的事件细分。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(check (fs-event-type (make-fs-event 'modified "/x/HEAD" #f)) => 'modified)
(check (fs-event-type (make-fs-event 'created "/x/HEAD" #f)) => 'created)
(check (fs-event-type (make-fs-event 'deleted "/x/HEAD" #f)) => 'deleted)
(check (fs-event-type (make-fs-event 'moved "/a/old" "/a/new")) => 'moved)

(check-report)
