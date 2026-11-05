;; pi-model-thinking-levels
;; 按模型元数据条目得出可用的思考级别列表。
;;
;; 语法
;; ----
;; (pi-model-thinking-levels entry)
;;
;; 参数
;; ----
;; entry : list?
;; 模型元数据条目。
;;
;; 返回值
;; ----
;; list?
;; 该模型所支持的思考级别字符串列表。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

;; 1. reasoning 为假：只支持 off
(check (pi-model-thinking-levels '(("id" . "plain") ("reasoning" . #f)))
  =>
  '("off")
) ;check
(check (pi-model-thinking-levels '(("id" . "plain") ("reasoning" . false)))
  =>
  '("off")
) ;check

;; 2. reasoning 为真：默认支持 off 到 high
(check (pi-model-thinking-levels '(("id" . "smart") ("reasoning" . #t)))
  =>
  '("off" "minimal" "low" "medium" "high")
) ;check

;; 3. 条目缺失 reasoning 字段：按不支持处理
(check (pi-model-thinking-levels '(("id" . "unknown"))) => '("off"))

;; 4. thinkingLevelMap 映射为 null 的级别不支持

(define %model-null-map
  (list (cons "id" "m")
    (cons "reasoning" #t)
    (cons "thinkingLevelMap" (list (cons "medium" 'null)))
  ) ;list
) ;define
(check (pi-model-thinking-levels %model-null-map)
  =>
  '("off" "minimal" "low" "high")
) ;check

;; 5. xhigh/max 需要显式映射才支持

(define %model-max-map
  (list (cons "id" "m")
    (cons "reasoning" #t)
    (cons "thinkingLevelMap" (list (cons "max" "max")))
  ) ;list
) ;define
(check (pi-model-thinking-levels %model-max-map)
  =>
  '("off" "minimal" "low" "medium" "high" "max")
) ;check

;; 6. 符号键与字符串键等价
(check (pi-model-thinking-levels '((id . "m") (reasoning . #t)))
  =>
  '("off" "minimal" "low" "medium" "high")
) ;check

(check-report)
