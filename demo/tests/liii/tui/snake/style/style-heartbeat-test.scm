;; style-heartbeat
;; 心跳指示字符染色（粗体紫色）
;;
;; 语法
;; ----
;; (style-heartbeat s)
;;
;; 参数
;; ----
;; s : string?\n待染色文本
;;
;; 返回值
;; ------
;; string?\n颜色开启时为 ANSI 着色文本，关闭时原样返回

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

(with-color-mode #f (lambda () (check (style-heartbeat "+") => "+")))

(with-color-mode #t
  (lambda ()
    (let ((s (style-heartbeat "+")))
      (check (display-width s) => 1)
      (check (string=? (strip-ansi s) "+") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

(check-catch 'type-error (style-heartbeat 9))

(check-report)
