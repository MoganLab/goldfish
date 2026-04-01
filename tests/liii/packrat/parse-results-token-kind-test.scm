(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-results-token-kind
;; 获取基础 token 的类型标识符。
;;
;; 语法
;; ----
;; (parse-results-token-kind results)
;;
;; 参数
;; ----
;; results : parse-results
;;   parse-results 对象
;;
;; 返回值
;; ----
;; kind-object or #f
;;   token 类型标识符，或 #f 表示输入结束
;;
;; 描述
;; ----
;; 从 parse-results 中提取当前 token 的类型（通常是符号）。
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
  (check (parse-results-token-kind results) => 'num)
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
) ;let

(let ()
  (define gen (let ((tokens '((oparen) (num . 1) (cparen))))
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
  (check (parse-results-token-kind results) => 'oparen)
) ;let

(let ()
  (define gen (lambda () (values #f #f)))
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => #f)
) ;let

(check-report)
