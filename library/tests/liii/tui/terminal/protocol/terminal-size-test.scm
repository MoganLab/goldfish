;; terminal-size
;; 查询终端的尺寸（宽度与高度）。
;;
;; 语法
;; ----
;; (terminal-size term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; pair?
;; 形如 (width . height) 的尺寸点对。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal-size vt) => '(20 5))
) ;let

(let* ((p (open-output-string))
       (st (make-standard-terminal :output-port p :detect-size-fn (lambda () '(100
                                                                               30)))
       ) ;st
      ) ;
  (check (terminal-size st) => '(100 30))
) ;let*

(check-catch 'type-error (terminal-size "not-a-terminal"))

(check-report)
