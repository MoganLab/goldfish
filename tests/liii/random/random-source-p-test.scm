(import (liii random)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; random-source?
;; 检查对象是否为随机源。
;;
;; 语法
;; ----
;; (random-source? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 当 obj 是随机源时返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (random-source? (make-random-source))  => #t
;; (random-source? 'not-a-source)         => #f
;; (random-source? default-random-source) => #t

; 随机源测试
(let ((s (make-random-source)))
  (check (random-source? s) => #t)
) ;let

; 非随机源测试
(check (random-source? 'not-a-source) => #f)
(check (random-source? 123) => #f)
(check (random-source? "string") => #f)
(check (random-source? '(1 2 3)) => #f)
(check (random-source? #t) => #f)
(check (random-source? 3.14) => #f)

; 默认随机源
(check (random-source? default-random-source) => #t)

(check-report)
