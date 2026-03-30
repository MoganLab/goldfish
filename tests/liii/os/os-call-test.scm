(import (liii check)
        (liii os)
        (scheme time)
) ;import

(check-set-mode! 'report-failed)

;; os-call
;; 执行系统命令。
;;
;; 语法
;; ----
;; (os-call command)
;;
;; 参数
;; ----
;; command : string?
;; 要执行的系统命令字符串。
;;
;; 说明
;; ----
;; 执行指定的系统命令并等待其完成。

;;; 基本功能测试
(when (not (os-windows?))
  (let ((t1 (current-second)))
    (os-call "sleep 1")
    (let ((t2 (current-second)))
      (check (>= (ceiling (- t2 t1)) 1) => #t)
    ) ;let
  ) ;let
) ;when

(check-report)
