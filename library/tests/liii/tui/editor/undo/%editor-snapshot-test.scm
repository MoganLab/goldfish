;; %editor-snapshot
;; 捕获编辑器的当前文本行、光标行列与粘贴注册表快照。
;;
;; 语法
;; ----
;; (%editor-snapshot data)
;;
;; 参数
;; ----
;; data : any
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; list?
;; 包含 (lines row col pastes-snap) 的快照列表。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
  (liii tui editor undo)
) ;import

(check-set-mode! 'report-failed)

(let* ((ed (make-text-input "hello"))
       (data (component-data ed))
       (snap (%editor-snapshot data))
      ) ;
  (check (list? snap) => #t)
  (check (list-ref snap 0) => '("hello"))
  (check (list-ref snap 1) => 0)
  (check (list-ref snap 2) => 5)
) ;let*

(check-report)
