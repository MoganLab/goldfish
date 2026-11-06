;; prepend-base
;; 构造带基础记号的解析结果流。
;;
;; 语法
;; ----
;; (prepend-base pos base next)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 当前位置。
;; base : pair 或 #f
;; 基础记号，形如 (记号类型 . 记号值)。
;; next : <parse-results> 或 procedure 或 #f
;; 后续解析状态。
;;
;; 返回值
;; ----
;; <parse-results>
;; 解析结果流记录。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test" 1 0)))
  (let ((results (prepend-base pos '(num . 42) #f)))
    (check-true (parse-results? results))
    (check (parse-results-position results) => pos)
    (check (parse-results-base results) => '(num . 42))
    (check (parse-results-token-kind results) => 'num)
    (check (parse-results-token-value results) => 42)
  ) ;let
) ;let

(check-report)
