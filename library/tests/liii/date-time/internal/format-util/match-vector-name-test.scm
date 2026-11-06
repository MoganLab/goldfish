;; match-vector-name
;; 在名称向量中查找输入串大小写不敏感的最长前缀匹配。
;;
;; 语法
;; ----
;; (match-vector-name input-str pos vec start-idx)
;;
;; 参数
;; ----
;; input-str : string? 输入串。
;; pos : integer? 起始位置。
;; vec : vector? 候选名称向量。
;; start-idx : integer? 起始检索下标。
;;
;; 返回值
;; ----
;; values integer? 命中下标与 integer? 结束位置；无匹配抛 value-error。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (match-vector-name "Monday" 0 LONG-WEEKDAY 0))
         list
       ) ;call-with-values
  =>
  '(1 6)
) ;check
(check (call-with-values (lambda () (match-vector-name "sunRISE" 0 ABBR-WEEKDAY 0))
         list
       ) ;call-with-values
  =>
  '(0 3)
) ;check
(check-catch 'value-error (match-vector-name "Xyz" 0 ABBR-WEEKDAY 0))

(check-report)
