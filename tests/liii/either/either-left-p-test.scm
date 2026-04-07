(import (liii check)
        (liii either)
) ;import

(check-set-mode! 'report-failed)

;; either-left?
;; 判断 Either 值是否为 Left。
;;
;; 语法
;; ----
;; (either-left? value)
;;
;; 参数
;; ----
;; value : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果 value 是 Left 状态的 Either 则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 非 Either 值返回 #f。
;;
;; 示例
;; ----
;; (either-left? (from-left "error")) => #t
;; (either-left? (from-right 1)) => #f
;;
;; 错误处理
;; ----
;; 无

(let ((left-val (from-left "error"))
      (right-val (from-right "success")))
  (check-true (either-left? left-val))
  (check-false (either-left? right-val))
) ;let

(check-false (either-left? '()))
(check-false (either-left? "string"))

(check-report)
