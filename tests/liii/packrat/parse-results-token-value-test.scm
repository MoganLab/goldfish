(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-results-token-value
;; 获取基础 token 的语义值。
;;
;; 语法
;; ----
;; (parse-results-token-value results)
;;
;; 参数
;; ----
;; results : parse-results
;;   parse-results 对象
;;
;; 返回值
;; ----
;; value-object or #f
;;   token 的语义值，或 #f 表示输入结束
;;
;; 描述
;; ----
;; 从 parse-results 中提取当前 token 的语义值（通常是具体数据）。
;; 对于没有语义值的 token（如括号），可能返回空列表。
;; 返回 #f 表示输入流已结束。

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
  (check (parse-results-token-value results) => 'foo)
) ;let

(let ()
  (define gen (let ((tokens '((string . "hello"))))
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
  (check (parse-results-token-value results) => "hello")
) ;let

(let ()
  (define gen (lambda () (values #f #f)))
  (define results (base-generator->results gen))
  (check (parse-results-token-value results) => #f)
) ;let

(check-report)
