;; %tool-now-ms
;; 获取工具卡片计时使用的当前单调毫秒时间戳。
;;
;; 语法
;; ----
;; (%tool-now-ms)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; number?
;; 非负单调时间戳（毫秒）。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views tool-card)
) ;import

(check-set-mode! 'report-failed)

(let ((t1 (%tool-now-ms)))
  (check-true (number? t1))
  (check-true (>= t1 0))
  (let ((t2 (%tool-now-ms)))
    (check-true (>= t2 t1))
  ) ;let
) ;let

(check-report)
