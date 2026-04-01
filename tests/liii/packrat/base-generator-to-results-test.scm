(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; base-generator->results
;; 将基础 token 生成器转换为 parse-results 对象。
;;
;; 语法
;; ----
;; (base-generator->results generator)
;;
;; 参数
;; ----
;; generator : procedure
;;   基础 token 生成函数，应返回两个值：parse-position 或 #f，以及 token 对或 #f
;;
;; 返回值
;; ----
;; parse-results
;;   表示从生成器读取的解析结果
;;
;; 描述
;; ----
;; 将简单的生成器函数包装成 packrat 解析器所需的 parse-results 对象。
;; 生成器每次被调用应返回两个值：位置信息和 token。

(let ()
  (define gen (lambda () (values (make-parse-position "test" 1 0) #f)))
  (define results (base-generator->results gen))
  (check-true (parse-results? results))
) ;let

(let ()
  (define gen (let ((tokens '((num . 100))))
                (lambda ()
                  (if (null? tokens)
                      (values #f #f)
                      (let ((token (car tokens)))
                        (set! tokens (cdr tokens))
                        (values #f token))
                      ) ;let
                  ) ;if
                ) ;lambda
  ) ;define
  (define results (base-generator->results gen))
  (check-true (parse-results? results))
  (check (parse-results-token-kind results) => 'num)
  (check (parse-results-token-value results) => 100)
) ;let

(let ()
  (define gen (let ((tokens '((id . foo) (num . 42))))
                (lambda ()
                  (if (null? tokens)
                      (values #f #f)
                      (let ((token (car tokens)))
                        (set! tokens (cdr tokens))
                        (values #f token))
                      ) ;let
                  ) ;if
                ) ;lambda
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => 'id)
  (check (parse-results-token-value results) => 'foo)
) ;let

(check-report)
