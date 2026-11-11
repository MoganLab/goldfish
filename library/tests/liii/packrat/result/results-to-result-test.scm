;; results->result
;; 按非终结符名查询或登记记忆化解析结果。
;;
;; 语法
;; ----
;; (results->result results key fn)
;;
;; 参数
;; ----
;; results : <parse-results>
;; 解析结果流记录。
;; key : symbol
;; 非终结符名。
;; fn : procedure
;; 未命中缓存时调用的过程，返回 <parse-result>。
;;
;; 返回值
;; ----
;; <parse-result>
;; 缓存中已有的解析结果，或本次新登记的解析结果。
;;
;; 说明
;; ----
;; 同一 key 再次查询时直接返回缓存结果，
;; 是 packrat 消除重复解析的核心机制。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

;; 未命中：调用 fn 并登记
(let* ((pos (make-parse-position "test" 1 0))
       (results (prepend-base pos '(num . 1) #f))
       (calls 0)
       (result
         (results->result results
           'expr
           (lambda () (set! calls (+ calls 1)) (make-result 42 #f))
         ) ;results->result
       ) ;result
      ) ;
  (check (parse-result-semantic-value result) => 42)
  (check calls => 1)
  ;; 再次查询命中缓存，不再调用 fn
  (let ((cached (results->result results 'expr (lambda () (make-result 0 #f)))))
    (check (parse-result-semantic-value cached) => 42)
    (check calls => 1)
  ) ;let
) ;let*

(check-report)
