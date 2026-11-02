;; screen-refresh!
;; 清屏并按当前帧内容强制全量重绘主屏幕。
;;
;; 语法
;; ----
;; (screen-refresh! screen)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; 返回值
;; ----
;; #t
;; 刷新完成返回 #t。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (screen-render! screen '("12345" "abcde"))
  (check (screen-lines screen) => '("12345" "abcde"))
  ;; 全量刷新后内容保持不变
  (check (screen-refresh! screen) => #t)
  (check (screen-lines screen) => '("12345" "abcde"))
  (check (virtual-terminal-screen vt)
    =>
    '("12345     " "abcde     " "          ")
  ) ;check
) ;let*

(check-catch 'type-error (screen-refresh! "not-a-screen"))

(check-report)
