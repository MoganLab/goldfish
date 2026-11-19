;; make-vt-state
;; 创建虚拟终端内部状态记录实例。
;;
;; 语法
;; ----
;; (make-vt-state width height cursor-x cursor-y visible? lines raw keys)
;;
;; 参数
;; ----
;; width : integer?
;; 宽度。
;;
;; height : integer?
;; 高度。
;;
;; cursor-x : integer?
;; 光标列。
;;
;; cursor-y : integer?
;; 光标行。
;;
;; visible? : boolean?
;; 光标可见性。
;;
;; lines : vector?
;; 屏幕行向量。
;;
;; raw : list?
;; 原始输出列表。
;;
;; keys : list?
;; 按键队列列表。
;;
;; 返回值
;; ----
;; vt-state?
;; 虚拟终端状态记录。

(import (scheme base)
  (liii check)
  (liii tui terminal virtual state)
) ;import

(check-set-mode! 'report-failed)

(let ((st (make-vt-state 80 24 0 0 #t '#() '() '())))
  (check-true (vt-state? st))
) ;let

(check-report)
