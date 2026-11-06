;; screen-resize!
;; 调整主屏幕的终端几何尺寸并按新尺寸全量重绘。
;;
;; 语法
;; ----
;; (screen-resize! screen cols rows)
;;
;; 参数
;; ----
;; screen : main-screen?
;; 主屏幕驱动实例。
;;
;; cols : integer?
;; 新的终端列数，必须为正整数。
;;
;; rows : integer?
;; 新的终端行数，必须为正整数。
;;
;; 返回值
;; ----
;; #t
;; 调整完成返回 #t。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui main-screen driver)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  ;; 初始渲染
  (screen-render! screen '("12345" "abcde"))
  (check (screen-lines screen) => '("12345" "abcde"))
  (check (virtual-terminal-screen vt)
    =>
    '("12345     " "abcde     " "          ")
  ) ;check

  ;; 调整尺寸 (resize)
  (check (screen-resize! screen 15 4) => #t)
  (check (terminal-size vt) => '(15 4))
  ;; 验证在新的几何尺寸下自适应全量重绘
  (check (virtual-terminal-screen vt)
    =>
    '("12345          " "abcde          " "               " "               ")
  ) ;check
) ;let*

;; 错误参数测试
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-main-screen vt))
      ) ;
  (check-catch 'type-error (screen-resize! screen 0 4))
  (check-catch 'type-error (screen-resize! screen 15 -1))
  (check-catch 'type-error (screen-resize! screen "15" 4))
) ;let*

(check-report)
