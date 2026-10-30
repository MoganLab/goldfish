(import (liii check)
        (liii tui mouse))

(check-set-mode! 'report-failed)

;; make-mouse-event
;; 解析 SGR 鼠标序列，构造鼠标事件。
;;
;; 语法
;; ----
;; (parse-mouse-event str)
;;
;; 参数
;; ----
;; str : string, SGR 编码的鼠标转义序列，如 "\x1b[<0;20;5M"
;;
;; 返回值
;; ----
;; mouse-event 或 #f
;; (make-mouse-event type button x y) 构造，访问器为 mouse-event-type /
;; mouse-event-button / mouse-event-x / mouse-event-y（坐标 0-based）。

;; 1. 左键按下：ESC[<0;20;5M -> 按钮左键，x=19, y=4（0-based）
(let ((e (parse-mouse-event "\x1b[<0;20;5M")))
  (check (mouse-event? e) => #t)
  (check (mouse-event-type e) => 'press)
  (check (mouse-event-button e) => 'left)
  (check (mouse-event-x e) => 19)
  (check (mouse-event-y e) => 4)
) ;let

;; 2. 左键释放：M -> m
(let ((e (parse-mouse-event "\x1b[<0;20;5m")))
  (check (mouse-event? e) => #t)
  (check (mouse-event-type e) => 'release)
  (check (mouse-event-button e) => 'left)
) ;let

;; 3. 中键(1)与右键(2)
(let ((e (parse-mouse-event "\x1b[<1;1;1M")))
  (check (mouse-event-button e) => 'middle)
) ;let
(let ((e (parse-mouse-event "\x1b[<2;1;1M")))
  (check (mouse-event-button e) => 'right)
) ;let

;; 4. 释放事件（final m）优先于 motion 位判定，按钮归一化仍取低 2 位
(let ((e (parse-mouse-event "\x1b[<32;3;3m")))
  (check (mouse-event-type e) => 'release)
  (check (mouse-event-button e) => 'left)
) ;let

;; 5. 滚轮：64=向上，65=向下
(let ((e (parse-mouse-event "\x1b[<64;10;10M")))
  (check (mouse-event-type e) => 'wheel-up)
  (check (mouse-event-button e) => 'none)
) ;let
(let ((e (parse-mouse-event "\x1b[<65;10;10M")))
  (check (mouse-event-type e) => 'wheel-down)
  (check (mouse-event-button e) => 'none)
) ;let

;; 6. 拖动/移动：32 位（按下后的 motion）
(let ((e (parse-mouse-event "\x1b[<32;4;6M")))
  (check (mouse-event-type e) => 'drag)
  (check (mouse-event-button e) => 'left)
) ;let
(let ((e (parse-mouse-event "\x1b[<35;4;6M")))
  (check (mouse-event-type e) => 'drag)
  (check (mouse-event-button e) => 'none)
) ;let

;; 7. 修饰键位
(let ((e (parse-mouse-event "\x1b[<4;1;1M")))
  (check (mouse-event-shift? e) => #t)
  (check (mouse-event-ctrl? e) => #f)
  (check (mouse-event-alt? e) => #f)
) ;let
(let ((e (parse-mouse-event "\x1b[<16;1;1M")))
  (check (mouse-event-ctrl? e) => #t)
) ;let
(let ((e (parse-mouse-event "\x1b[<8;1;1M")))
  (check (mouse-event-alt? e) => #t)
) ;let

;; 8. 非鼠标序列返回 #f
(check (parse-mouse-event "\x1b[A") => #f)
(check (parse-mouse-event "a") => #f)
(check (parse-mouse-event "") => #f)
(check (parse-mouse-event "\x1b[<0;20M") => #f)

;; 9. 类型检查
(check-catch 'type-error (parse-mouse-event 123))
(check-catch 'type-error (parse-mouse-event 'a))
(check (mouse-event? "not-event") => #f)
(check (mouse-event? 42) => #f)

;; 10. 事件到达时刻（Issue #1382）：滚轮加速依赖真实到达间隔，
;;     合并处理连续事件时须保留各事件自己的时间
(let ((e (make-mouse-event 'wheel-up 'none 5 5 #f #f #f 1000))
      (no-time (make-mouse-event 'wheel-up 'none 5 5)))
  (check (mouse-event-time e) => 1000)
  ;; 未提供时刻时为 #f，调用方回退到处理时刻
  (check (mouse-event-time no-time) => #f)
  ;; 时刻不影响其他字段
  (check (mouse-event-type e) => 'wheel-up)
  (check (mouse-event-x e) => 5)
  (check (mouse-event-y e) => 5)
) ;let

;; 11. parse-mouse-event 的可选时刻参数写入事件，省略时为 #f
(let ((e (parse-mouse-event "\x1b;[<64;6;3M" 2000))
      (no-time (parse-mouse-event "\x1b;[<64;6;3M")))
  (check (mouse-event-time e) => 2000)
  (check (mouse-event-type e) => 'wheel-up)
  (check (mouse-event-time no-time) => #f)
) ;let

(check-report)
