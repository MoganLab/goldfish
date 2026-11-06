;; fs-event-dest-path
;; 读取文件系统事件记录的目标路径（仅 "moved" 事件有值）。
;;
;; 语法
;; ----
;; (fs-event-dest-path event)
;;
;; 参数
;; ----
;; event : fs-event?
;; 事件记录。
;;
;; 返回值
;; ----
;; string 或 #f
;; "moved" 事件返回移动后的目标路径，其余事件返回 #f。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

;; moved 事件携带目标路径
(check (fs-event-dest-path (make-fs-event 'moved "/a/old" "/a/new")) => "/a/new")

;; 其余事件没有目标路径
(check-false (fs-event-dest-path (make-fs-event 'modified "/x/HEAD" #f)))
(check-false (fs-event-dest-path (make-fs-event 'created "/x/HEAD" #f)))
(check-false (fs-event-dest-path (make-fs-event 'deleted "/x/HEAD" #f)))

(check-report)
