(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-result-semantic-value
;; 获取成功解析的语义值。
;;
;; 语法
;; ----
;; (parse-result-semantic-value result)
;;
;; 参数
;; ----
;; result : parse-result
;;   解析结果对象
;;
;; 返回值
;; ----
;; any
;;   若解析成功则返回语义值，否则返回 #f
;;
;; 描述
;; ----
;; 从成功的解析结果中提取语义值。语义值是解析过程中
;; 通过计算得到的实际数据。

(let ()
  (define success (make-result 42 #f))
  (check (parse-result-semantic-value success) => 42)
) ;let

(let ()
  (define success2 (make-result "hello world" #f))
  (check (parse-result-semantic-value success2) => "hello world")
) ;let

(let ()
  (define success3 (make-result '(a b c) #f))
  (check (parse-result-semantic-value success3) => '(a b c))
) ;let

(let ()
  (define fail (make-expected-result (make-parse-position #f 1 0) "num"))
  (check (parse-result-semantic-value fail) => #f)
) ;let

(check-report)
