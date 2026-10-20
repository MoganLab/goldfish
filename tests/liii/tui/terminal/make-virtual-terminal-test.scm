(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; make-virtual-terminal
;; 创建一个内存虚拟终端实例，用于脱机测试。
;;
;; 语法
;; ----
;; (make-virtual-terminal [:width w] [:height h])
;; (make-virtual-terminal w h)
;;
;; 参数
;; ----
;; w : integer, 可选，终端列数（宽度），默认为检测尺寸的列数（兜底 80）。
;; h : integer, 可选，终端行数（高度），默认为检测尺寸的行数（兜底 24）。
;;
;; 返回值
;; ----
;; terminal
;; 虚拟终端对象。

;; 使用关键字参数构造
(let ((vt (make-virtual-terminal :width 40 :height 10)))
  (check (terminal? vt) => #t)
  (check (virtual-terminal? vt) => #t)
  (check (terminal-size vt) => '(40 10))
  (check (terminal-cursor vt) => '(0 0))
  (check (terminal-cursor-visible? vt) => #t)
  ;; 屏幕行数应为 10，每行 40 空格
  (let ((screen (virtual-terminal-screen vt)))
    (check (length screen) => 10)
    (check (car screen) => (make-string 40 #\space)))
  ;; 原始日志为空列表
  (check (virtual-terminal-raw vt) => '()))

;; 使用位置参数构造
(let ((vt (make-virtual-terminal 30 15)))
  (check (terminal? vt) => #t)
  (check (virtual-terminal? vt) => #t)
  (check (terminal-size vt) => '(30 15)))

;; 无参构造（默认尺寸）
(let ((vt (make-virtual-terminal)))
  (check (terminal? vt) => #t)
  (check (virtual-terminal? vt) => #t)
  (let ((sz (terminal-size vt)))
    (check (> (car sz) 0) => #t)
    (check (> (cadr sz) 0) => #t)))

;; 非法参数校验
(check-catch 'type-error (make-virtual-terminal "40" 10))
(check-catch 'type-error (make-virtual-terminal 40 "10"))
(check-catch 'value-error (make-virtual-terminal 0 10))
(check-catch 'value-error (make-virtual-terminal 40 -1))

(check-report)
