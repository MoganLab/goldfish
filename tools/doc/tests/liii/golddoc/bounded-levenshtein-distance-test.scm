;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check) (liii golddoc))

(check-set-mode! 'report-failed)

;; bounded-levenshtein-distance
;; 计算两个函数名之间的有界编辑距离。
;;
;; 语法
;; ----
;; (bounded-levenshtein-distance left right)
;;
;; 参数
;; ----
;; left : string?
;; right : string?
;;
;; 返回值
;; ----
;; integer? | boolean?
;; 当编辑距离小于等于 `max-fuzzy-edit-distance` 时返回距离，否则返回 `#f`。
;;
;; 描述
;; ----
;; `gf doc` 的模糊匹配固定使用编辑距离阈值 `2`，
;; 超过该阈值的候选不会进入建议列表。

(check max-fuzzy-edit-distance => 2)

(check (bounded-levenshtein-distance "string-splst"
         "string-split"
       ) ;bounded-levenshtein-distance
  =>
  1
) ;check

(check (bounded-levenshtein-distance "string-spilt"
         "string-split"
       ) ;bounded-levenshtein-distance
  =>
  2
) ;check

(check (bounded-levenshtein-distance "string-split"
         "string-split"
       ) ;bounded-levenshtein-distance
  =>
  0
) ;check

(check (bounded-levenshtein-distance "string-splst"
         "hash-table"
       ) ;bounded-levenshtein-distance
  =>
  #f
) ;check

(check-report)
