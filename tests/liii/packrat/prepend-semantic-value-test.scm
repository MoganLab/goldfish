;; prepend-semantic-value
;; 构造带记忆化语义值的解析结果流。
;;
;; 语法
;; ----
;; (prepend-semantic-value pos key result next)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 当前位置。
;; key : symbol
;; 记忆化缓存键（非终结符名）。
;; result : any
;; 缓存的语义值。
;; next : <parse-results> 或 procedure 或 #f
;; 后续解析状态。
;;
;; 返回值
;; ----
;; <parse-results>
;; 解析结果流记录，其记忆化表登记了 key。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test" 1 0)))
  (let ((results (prepend-semantic-value pos 'expr 42 #f)))
    (check-true (parse-results? results))
    (check (parse-results-position results) => pos)
    (check-false (parse-results-base results))
    ;; 通过 results->result 命中缓存
    (check (results->result results 'expr (lambda () (make-result 0 #f)))
      => (make-result 42 #f))
  ) ;let
) ;let

(check-report)
