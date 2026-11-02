;; parse-mouse-event
;; 解析 SGR 编码的鼠标 ANSI 转义序列为鼠标事件对象。
;;
;; 语法
;; ----
;; (parse-mouse-event str [time])
;;
;; 参数
;; ----
;; str : string?
;; SGR 编码的鼠标序列，形如 "\x1b[<0;20;5M"。
;;
;; time : number? (可选，默认为 #f)
;; 可选的时间戳信息。
;;
;; 返回值
;; ----
;; mouse-event? 或 #f
;; 若解析成功返回 mouse-event 对象，否则返回 #f。

(import (scheme base) (liii check) (liii tui mouse))

(check-set-mode! 'report-failed)

;; 1. 左键按下与释放
(let ((e (parse-mouse-event "\x1b;[<0;20;5M")))
  (check-true (mouse-event? e))
  (check (mouse-event-type e) => 'press)
  (check (mouse-event-button e) => 'left)
  (check (mouse-event-x e) => 19)
  (check (mouse-event-y e) => 4)
) ;let

(let ((e (parse-mouse-event "\x1b;[<0;20;5m")))
  (check (mouse-event-type e) => 'release)
  (check (mouse-event-button e) => 'left)
) ;let

;; 2. 中键与右键
(let ((e (parse-mouse-event "\x1b;[<1;1;1M")))
  (check (mouse-event-button e) => 'middle)
) ;let
(let ((e (parse-mouse-event "\x1b;[<2;1;1M")))
  (check (mouse-event-button e) => 'right)
) ;let

;; 3. 滚轮
(let ((e (parse-mouse-event "\x1b;[<64;10;10M")))
  (check (mouse-event-type e) => 'wheel-up)
  (check (mouse-event-button e) => 'none)
) ;let
(let ((e (parse-mouse-event "\x1b;[<65;10;10M")))
  (check (mouse-event-type e) => 'wheel-down)
  (check (mouse-event-button e) => 'none)
) ;let

;; 4. 非法输入返回 #f
(check-false (parse-mouse-event "\x1b;[A"))
(check-false (parse-mouse-event "abc"))
(check-false (parse-mouse-event ""))

;; 5. 类型错误
(check-catch 'type-error (parse-mouse-event 123))

(check-report)
