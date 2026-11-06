;; format-datum+node
;; 格式化一个 Scheme datum，并同时返回带位置信息的新 node。
;;
;; 语法
;; ----
;; (format-datum+node datum)
;;
;; 参数
;; ----
;; datum : any?
;; 已经由 `read` 读出的 Scheme datum。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：
;; 1. text : string? 格式化后的文本
;; 2. positioned-node : env? 或 atom? 带有布局位置信息的新 node
;;
;; 说明
;; ----
;; `format-datum+node` 是 `format-datum` 的信息保留版本，适合测试和后续需要定位信息的功能。

(import (liii check)
  (liii goldfmt format)
  (liii goldfmt record)
  (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

(call-with-values
  (lambda () (format-datum+node '(define (f x) (+ x 1))))
  (lambda (text node)
    (check text
      =>
      (&- #""
                 (define (f x)
                   (+ x 1)
                 ) ;define
                 ""
      ) ;&-
    ) ;check
    (check (env-indent node) => 0)
    (check (env-left-line node) => 1)
    (check (env-right-line node) => 3)
  ) ;lambda
) ;call-with-values

(check-report)
