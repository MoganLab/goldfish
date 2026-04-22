(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cdar
;; 返回列表首元素的尾元素。
;;
;; 语法
;; ----
;; (cdar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 一个点对，其 car 必须也是一个点对。
;;
;; 返回值
;; ------
;; any
;; (car pair) 的 cdr。
;;
;; 说明
;; ----
;; (cdar x) 等价于 (cdr (car x))。
(check (cdar '((a b) c)) => '(b))
(check (cdar '((a b c) d)) => '(b c))
(check (cdar '((a) b)) => '())
(check (cdar '(((a) b) c)) => '(b))
(check-catch 'wrong-type-arg (cdar '()))
(check-catch 'wrong-type-arg (cdar 'a))
(check-catch 'wrong-number-of-args
  (cdar)
) ;check-catch
(check-catch 'wrong-number-of-args
  (cdar '((a b) c) 'x)
) ;check-catch

(check-report)
