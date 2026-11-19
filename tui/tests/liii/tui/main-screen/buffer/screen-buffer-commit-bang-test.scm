;; screen-buffer-commit!
;; 提交一帧内容到屏幕行缓冲区，并将当前帧滚动到上一帧。
;;
;; 语法
;; ----
;; (screen-buffer-commit! buf new-lines)
;;
;; 参数
;; ----
;; buf : screen-buffer?
;; 屏幕行缓冲区实例。
;;
;; new-lines : list?
;; 新的当前帧文本行列表。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  ;; 第一帧提交
  (screen-buffer-commit! buf '("line 1" "line 2"))
  (check (screen-buffer-lines buf) => '("line 1" "line 2"))
  (check (screen-buffer-prev-lines buf) => '())
  (check-false (screen-buffer-dirty? buf))
  ;; 第二帧提交
  (screen-buffer-commit! buf '("modified 1"))
  (check (screen-buffer-lines buf) => '("modified 1"))
  (check (screen-buffer-prev-lines buf) => '("line 1" "line 2"))
  (check-false (screen-buffer-dirty? buf))
) ;let

(let ((buf (make-screen-buffer)))
  (check-catch 'type-error (screen-buffer-commit! buf "not-a-list"))
) ;let

(check-report)
