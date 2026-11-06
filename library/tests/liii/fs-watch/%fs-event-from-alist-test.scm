;; %fs-event-from-alist
;; 把 C 层事件 alist 转换为 fs-event 记录。
;;
;; 语法
;; ----
;; (%fs-event-from-alist e)
;;
;; 参数
;; ----
;; e : list
;; C 层事件 alist，形如 ((type . modified) (path . "/x") [(dest . "/y")])。
;;
;; 返回值
;; ----
;; fs-event?
;; 事件记录；缺少 dest 字段时目标路径为 #f。
;;
;; 说明
;; ----
;; 本函数是 (liii fs-watch) 的实现细节，
;; 导出仅为让 (liii go) worker 会话中的 fs-watch-pump 可以引用。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

;; 基本转换
(let ((ev (%fs-event-from-alist '((type . modified) (path . "/x/HEAD")))))
  (check-true (fs-event? ev))
  (check (fs-event-type ev) => 'modified)
  (check (fs-event-path ev) => "/x/HEAD")
  (check-false (fs-event-dest-path ev))
) ;let

;; 带 dest 字段的 moved 事件
(let ((ev (%fs-event-from-alist '((type . moved) (path . "/a/old") (dest . "/a/new")))))
  (check (fs-event-type ev) => 'moved)
  (check (fs-event-path ev) => "/a/old")
  (check (fs-event-dest-path ev) => "/a/new")
) ;let

(check-report)
