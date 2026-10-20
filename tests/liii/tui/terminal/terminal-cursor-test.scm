(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; terminal-cursor & 光标控制与清屏接口
;;
;; 语法
;; ----
;; (terminal-cursor term)
;; (terminal-set-cursor! term x y)
;; (terminal-clear term)
;; (terminal-clear-line term)
;; (terminal-hide-cursor! term)
;; (terminal-show-cursor! term)
;; (terminal-cursor-visible? term)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  ;; 初始光标
  (check (terminal-cursor vt) => '(0 0))
  (check (terminal-cursor-visible? vt) => #t)

  ;; 正常移动
  (terminal-set-cursor! vt 5 2)
  (check (terminal-cursor vt) => '(5 2))

  ;; 在移动后的位置写入
  (terminal-write vt "ok")
  (check (terminal-cursor vt) => '(7 2))
  (check (list-ref (virtual-terminal-screen vt) 2) => "     ok             ")

  ;; 超出边界自动钳位
  (terminal-set-cursor! vt -5 -10)
  (check (terminal-cursor vt) => '(0 0))
  (terminal-set-cursor! vt 100 100)
  (check (terminal-cursor vt) => '(19 4))

  ;; 清除当前行
  (terminal-set-cursor! vt 0 2)
  (terminal-clear-line vt)
  (check (list-ref (virtual-terminal-screen vt) 2) => (make-string 20 #\space))

  ;; 整体清屏
  (terminal-set-cursor! vt 1 1)
  (terminal-write vt "data")
  (terminal-clear vt)
  (check (terminal-cursor vt) => '(0 0))
  (check (car (virtual-terminal-screen vt)) => (make-string 20 #\space))
  (check (cadr (virtual-terminal-screen vt)) => (make-string 20 #\space))

  ;; 光标显隐控制
  (terminal-hide-cursor! vt)
  (check (terminal-cursor-visible? vt) => #f)
  (terminal-show-cursor! vt)
  (check (terminal-cursor-visible? vt) => #t))

;; 类型校验
(check-catch 'type-error (terminal-set-cursor! "not-term" 0 0))
(check-catch 'type-error (terminal-set-cursor! (make-virtual-terminal :width 10 :height 5) "0" 0))

(check-report)
