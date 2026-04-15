(import (liii check)
  (liii base)
  (liii njson)
) ;import


(check-set-mode! 'report-failed)


;; njson?
;; 判断一个值在结构上是否是 njson 句柄。
;;
;; 语法
;; ----
;; (njson? x)
;;
;; 参数
;; ----
;; x : any
;; 任意 Scheme 值。
;;
;; 返回值
;; ----
;; boolean?
;; 句柄返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 这个谓词只检查结构，不负责判断句柄是否已经释放。
;;
;; 错误处理
;; ----
;; 无


(let-njson ((root (string->njson "{\"k\":1}")))
  (check-true (njson? root))
) ;let-njson


(check-false (njson? 'foo))
(check-false (njson? 1))
(check-false (njson? '(njson-handle . 1))
) ;check-false


(check-report)
