;; pi-clamp-thinking-level
;; 将请求的思考级别收敛到支持列表中最接近的级别。
;;
;; 语法
;; ----
;; (pi-clamp-thinking-level requested-level available-levels)
;;
;; 参数
;; ----
;; requested-level : string?
;; 请求的思考级别。
;;
;; available-levels : list?
;; 模型支持的思考级别字符串列表。
;;
;; 返回值
;; ----
;; string?
;; 收敛后的思考级别字符串（优先向高收敛，无更高档时向低收敛）。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

;; 1. 支持的级别原样返回
(check (pi-clamp-thinking-level "high" '("off" "minimal" "low" "medium" "high"))
  =>
  "high"
) ;check

;; 2. 请求级别不支持时先向更高档找
(check (pi-clamp-thinking-level "medium" '("off" "minimal" "low" "high"))
  =>
  "high"
) ;check

;; 3. 更高档都没有时向更低档找
(check (pi-clamp-thinking-level "high" '("off" "minimal" "low")) => "low")

;; 4. 只支持 off 的模型收敛为 off
(check (pi-clamp-thinking-level "high" '("off")) => "off")

;; 5. 级别不在全集内时取可用集首项
(check (pi-clamp-thinking-level "ludicrous" '("off" "minimal")) => "off")

;; 6. 可用集为空时回退 off
(check (pi-clamp-thinking-level "high" '()) => "off")

;; 7. off 本身也可用时保持 off
(check (pi-clamp-thinking-level "off" '("off" "minimal")) => "off")

(check-report)
