;; make-fs-event
;; 构造文件系统事件记录（对齐 Python watchdog 的 FileSystemEvent 可实例化语义）。
;;
;; 语法
;; ----
;; (make-fs-event type path)
;; (make-fs-event type path dest)
;;
;; 参数
;; ----
;; type : symbol?
;; 事件类型：'modified / 'created / 'deleted / 'moved。
;; path : string?
;; 事件目标路径。
;; dest : string? 或 #f (可选，默认 #f)
;; 仅 "moved" 事件的目标路径。
;;
;; 返回值
;; ----
;; fs-event?
;; 文件系统事件记录，可用 fs-event-type / fs-event-path / fs-event-dest-path 读取。
;;
;; 错误处理
;; ----
;; type 非符号、path 非字符串、dest 非字符串且非 #f 时抛出 type-error。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

;; 基本构造：modified 事件不带目标路径
(let ((ev (make-fs-event 'modified "/x/HEAD" #f)))
  (check-true (fs-event? ev))
  (check (fs-event-type ev) => 'modified)
  (check (fs-event-path ev) => "/x/HEAD")
  (check-false (fs-event-dest-path ev))
) ;let

;; moved 事件携带目标路径
(let ((ev (make-fs-event 'moved "/a/old" "/a/new")))
  (check (fs-event-type ev) => 'moved)
  (check (fs-event-path ev) => "/a/old")
  (check (fs-event-dest-path ev) => "/a/new")
) ;let

;; dest 省略时为 #f
(check-false (fs-event-dest-path (make-fs-event 'created "/a/b")))

;; 构造器参数校验
(check-catch 'type-error (make-fs-event 123 "/x"))
(check-catch 'type-error (make-fs-event 'modified 456))
(check-catch 'type-error (make-fs-event 'modified "/x" 789))

(check-report)
