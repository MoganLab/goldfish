;; screen-buffer-dirty-set!
;; 设置屏幕行缓冲区的脏标记。
;;
;; 语法
;; ----
;; (screen-buffer-dirty-set! buf val)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; val : any
;; 新的脏标记值，真值表示脏。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (screen-buffer-dirty-set! buf #f)
  (check-false (screen-buffer-dirty? buf))
  (screen-buffer-dirty-set! buf #t)
  (check-true (screen-buffer-dirty? buf))
) ;let

(check-catch 'type-error (screen-buffer-dirty-set! "not-a-buffer" #f))

(check-report)
