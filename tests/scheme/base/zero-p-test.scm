(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; zero?
;; 判断一个数值是否为零。
;;
;; 语法
;; ----
;; (zero? obj)
;;
;; 参数
;; ----
;; obj : number?
;; 任意数值类型。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值，当其为零时返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是数值类型
(check-true (zero? 0))
(check-true (zero? 0.0))
(check-true (zero? 0.0))
(check-true (zero? 0))
(check-false (zero? 1))
(check-false (zero? 1.0))
(check-false (zero? -1))
(check-false (zero? -1.0))
(check-false (zero? +inf.0))
(check-false (zero? -inf.0))
(check-false (zero? +nan.0))
(check-false (zero? 1.0+1.0i))
(check-catch 'wrong-type-arg
  (zero? #\A)
) ;check-catch
(check-catch 'wrong-type-arg (zero? #t))
(check-catch 'wrong-type-arg (zero? #f))
(check-catch 'wrong-type-arg
  (zero? "not-a-number")
) ;check-catch
(check-catch 'wrong-type-arg
  (zero? 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (zero? '(1 2 3))
) ;check-catch
(check-report)