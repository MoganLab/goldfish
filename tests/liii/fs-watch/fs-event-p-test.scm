(import (scheme base) (liii check) (liii string) (liii fs-watch))

(check-set-mode! 'report-failed)

;; make-fs-event
;; 构造文件系统事件记录（对齐 Python watchdog 的 FileSystemEvent 可实例化语义）。
;;
;; 语法
;; ----
;; (make-fs-event type path [:dest #f]) → fs-event?
;;
;; 参数
;; ----
;; type : string?
;; 事件类型："modified" / "created" / "deleted" / "moved"。
;; path : string?
;; 事件目标路径。
;; :dest : string? | #f (可选，默认 #f)
;; 仅 "moved" 事件的目标路径。
;;
;; 错误处理
;; ----
;; type 或 path 非字符串、dest 非 string 且非 #f 时抛出 type-error。

;; fs-event?
;; 判断对象是否为文件系统事件记录，并验证事件访问器。
;;
;; 语法
;; ----
;; (fs-event? obj) → boolean?
;; (fs-event-type event) → string?
;; (fs-event-path event) → string?
;; (fs-event-dest-path event) → string? | #f
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;; event : fs-event?
;; 事件记录。
;;
;; 返回值
;; -----
;; fs-event? 返回是否为 <fs-event> 记录；fs-event-type 返回事件类型字符串
;; （"modified" / "created" / "deleted" / "moved"）；fs-event-path 返回事件
;; 目标绝对路径；fs-event-dest-path 仅 "moved" 事件返回目标路径，其余 #f。
;;
;; 说明
;; ----
;; 1. macOS kqueue 后端只产生目录级 "modified" 事件（path 为监听目录本身）；
;;    Windows/Linux 提供具体文件的事件细分。

(let ((ev (make-fs-event "modified" "/x/HEAD" #f)))
  (check-true (fs-event? ev))
  (check (fs-event-type ev) => "modified")
  (check (fs-event-path ev) => "/x/HEAD")
  (check-false (fs-event-dest-path ev))
) ;let

;; moved 事件携带 dest
(let ((ev (make-fs-event "moved" "/a/old" "/a/new")))
  (check (fs-event-type ev) => "moved")
  (check (fs-event-path ev) => "/a/old")
  (check (fs-event-dest-path ev) => "/a/new")
) ;let

(check-false (fs-event? "not-event"))

;; 构造器参数校验
(check-catch 'type-error (make-fs-event 123 "/x"))
(check-catch 'type-error (make-fs-event "modified" 456))

(check-report)
