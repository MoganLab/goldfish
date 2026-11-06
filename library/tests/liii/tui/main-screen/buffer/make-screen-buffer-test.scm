;; make-screen-buffer
;; 创建一个屏幕行缓冲区实例，用于记录当前帧与上一帧的文本行。
;;
;; 语法
;; ----
;; (make-screen-buffer [init-lines])
;;
;; 参数
;; ----
;; init-lines : list? (可选，默认为 '())
;; 初始化的当前帧行列表。
;;
;; 返回值
;; ----
;; screen-buffer?
;; 创建的屏幕行缓冲区实例。

(import (scheme base) (liii check) (liii tui main-screen buffer))

(check-set-mode! 'report-failed)

(let ((buf (make-screen-buffer)))
  (check-true (screen-buffer? buf))
  (check (screen-buffer-lines buf) => '())
  (check (screen-buffer-prev-lines buf) => '())
  (check-true (screen-buffer-dirty? buf))
) ;let

(let ((buf (make-screen-buffer '("init 1" "init 2"))))
  (check-true (screen-buffer? buf))
  (check (screen-buffer-lines buf) => '("init 1" "init 2"))
  (check (screen-buffer-prev-lines buf) => '())
  (check-true (screen-buffer-dirty? buf))
) ;let

(check-catch 'type-error (make-screen-buffer "not-a-list"))

(check-report)
