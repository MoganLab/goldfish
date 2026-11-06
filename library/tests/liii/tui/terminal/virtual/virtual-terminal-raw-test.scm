;; virtual-terminal-raw
;; 获取虚拟终端记录的全部原始输出片段列表。
;;
;; 语法
;; ----
;; (virtual-terminal-raw vt)
;;
;; 参数
;; ----
;; vt : virtual-terminal?
;; 虚拟终端实例。
;;
;; 返回值
;; ----
;; list?
;; 原始输出片段的字符串列表。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 10 :height 3)))
  ;; 初始为空
  (check (virtual-terminal-raw vt) => '())
  (terminal-write vt "one")
  (terminal-write vt "two")
  (check (virtual-terminal-raw vt) => '("one" "two"))
) ;let

(check-catch 'type-error (virtual-terminal-raw "not-a-vt"))

(check-report)
