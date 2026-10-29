(import (liii check)
        (liii tui alt-screen)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; make-alt-screen 与属性访问
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt)))
  (check (alt-screen? screen) => #t)
  (check (alt-screen-terminal screen) => vt)
  (check (alt-screen-lines screen) => '())
) ;let

;; 类型检查
(check-catch 'type-error (make-alt-screen "not-a-terminal"))
(check-catch 'type-error (make-alt-screen 123))

;; 整帧渲染：行内容按绝对行号落位，未使用的行补空行
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt)))
  (check (alt-screen-render! screen '("line1" "line2") '(0 . 0)) => #t)
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "line1               ")
    (check (cadr lines) => "line2               ")
    (check (caddr lines) => "                    ")
    (check (list-ref lines 3) => "                    ")
    (check (list-ref lines 4) => "                    "))
  ;; 帧内容已按终端高度补齐
  (check (alt-screen-lines screen) => '("line1" "line2" "" "" ""))
  ;; 光标定位到指定位置
  (check (terminal-cursor vt) => '(0 0))
) ;let

;; 增量重绘：只有变化的行被重写，其余行保持不变
(let* ((vt (make-virtual-terminal :width 20 :height 4))
       (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("a" "b" "c" "d") '(0 . 0))
  (alt-screen-render! screen '("a" "X" "c" "d") '(0 . 0))
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "a                   ")
    (check (cadr lines) => "X                   ")
    (check (caddr lines) => "c                   ")
    (check (list-ref lines 3) => "d                   "))
  (check (alt-screen-lines screen) => '("a" "X" "c" "d"))
) ;let

;; 行内容变短时清掉行尾残留
(let* ((vt (make-virtual-terminal :width 10 :height 2))
       (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("abcdefghij" "second") '(0 . 0))
  (alt-screen-render! screen '("ab" "second") '(0 . 0))
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "ab        ")
    (check (cadr lines) => "second    "))
) ;let

;; 光标定位：传入 (x . y) 时把光标移到对应位置
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("hello") '(3 . 2))
  (check (terminal-cursor vt) => '(3 2))
) ;let

;; 清屏
(let* ((vt (make-virtual-terminal :width 10 :height 3))
       (screen (make-alt-screen vt)))
  (alt-screen-render! screen '("x" "y" "z") '(0 . 0))
  (check (alt-screen-clear! screen) => #t)
  (check (alt-screen-lines screen) => '())
  (let ((lines (virtual-terminal-screen vt)))
    (check (car lines) => "          ")
    (check (cadr lines) => "          ")
    (check (caddr lines) => "          "))
) ;let

;; 类型检查
(let ((vt (make-virtual-terminal :width 10 :height 3)))
  (check-catch 'type-error (alt-screen-render! 123 '("a")))
  (check-catch 'type-error (alt-screen-lines "not-a-screen"))
  (check-catch 'type-error (alt-screen-clear! 42))
) ;let

(check-report)
