;; terminal-max-y
;; 查询终端曾渲染到达的最大行索引（0-based）。
;;
;; 用于退出 TUI 时把光标定位到整个渲染块的下方，避免 shell prompt 画错位置。
;;
;; 语法
;; ----
;; (terminal-max-y term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; integer?
;; 最大行索引；未提供实现时回退为当前光标行。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui terminal protocol)
) ;import

(check-set-mode! 'report-failed)

;; virtual-terminal: max-y 至少不小于当前光标行
(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal-max-y vt) => 0)
  (terminal-set-cursor! vt 0 3)
  (check (terminal-max-y vt) => 3)
) ;let

;; 自定义记录：max-y 由实现过程提供
(let ((term (make-terminal-record 'custom
              (lambda (t) '(80 24))
              (lambda (t text) #t)
              (lambda (t) '(0 10))
              (lambda (t x y) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t) #t)
              (lambda (t v) #t)
              (lambda (t) #t)
              (lambda (t) 42)
              'payload
            ) ;make-terminal-record
      ) ;term
     ) ;
  (check (terminal-max-y term) => 42)
) ;let

(check-catch 'type-error (terminal-max-y "not-a-terminal"))

(check-report)
