(import (liii check)
  (liii pi ai thinking))

(check-set-mode! 'report-failed)

;; pi-thinking-levels
;; 思考级别全集（由弱到强）。
(define %levels
  '("off" "minimal" "low" "medium" "high" "xhigh" "max"))

(check pi-thinking-levels => %levels)

;; pi-default-thinking-level
;; 用户未显式指定时的默认级别（对齐上游 DEFAULT_THINKING_LEVEL）。
(check pi-default-thinking-level => "medium")

;; pi-thinking-level?
;; 判断级别是否在级别全集内。
(check-true (pi-thinking-level? "off"))
(check-true (pi-thinking-level? "max"))
(check-false (pi-thinking-level? "ludicrous"))
(check-false (pi-thinking-level? #f))
(check-false (pi-thinking-level? 42))

;; pi-json-true?
;; JSON 布尔归一：string->json 产出符号 true/false，内置目录则为 #t/#f。
(check-true (pi-json-true? #t))
(check-true (pi-json-true? 'true))
(check-false (pi-json-true? #f))
(check-false (pi-json-true? 'false))
(check-false (pi-json-true? 'null))
(check-false (pi-json-true? '()))

;; pi-model-supports-reasoning?
;; 模型是否支持 reasoning：条目缺失时按支持处理，字段缺失时按不支持处理。
(check-true (pi-model-supports-reasoning? '(("reasoning" . #t))))
(check-true (pi-model-supports-reasoning? '((reasoning . true))))
(check-false (pi-model-supports-reasoning? '((reasoning . false))))
(check-false (pi-model-supports-reasoning? '((reasoning . #f))))
(check-false (pi-model-supports-reasoning? '((reasoning . null))))
(check-false (pi-model-supports-reasoning? '(("id" . "m"))))
(check-true (pi-model-supports-reasoning? '()))

;; pi-model-thinking-levels
;; 按模型目录条目得出可用级别集（入参为条目 alist）。
;; 1. reasoning 为假：只支持 off（#f 与 JSON 符号 false 都要识别）
(check (pi-model-thinking-levels '(("id" . "plain") ("reasoning" . #f)))
  => '("off"))
(check (pi-model-thinking-levels '(("id" . "plain") ("reasoning" . false)))
  => '("off"))
;; 2. reasoning 为真：支持全集（xhigh/max 需显式映射，见下）
(check (pi-model-thinking-levels '(("id" . "smart") ("reasoning" . #t)))
  => '("off" "minimal" "low" "medium" "high"))
;; 3. 条目缺失 reasoning 字段：按不支持思考处理（条目本身缺失才算「无信息」）
(check (pi-model-thinking-levels '(("id" . "unknown"))) => '("off"))
;; 4. thinkingLevelMap 映射为 null 的级别不支持
(define %model-null-map
  (list (cons "id" "m")
    (cons "reasoning" #t)
    (cons "thinkingLevelMap" (list (cons "medium" 'null))))
)
(check (pi-model-thinking-levels %model-null-map)
  => '("off" "minimal" "low" "high"))
;; 5. xhigh/max 需要显式映射才支持
(check (pi-model-thinking-levels '(("id" . "m") ("reasoning" . #t)))
  => '("off" "minimal" "low" "medium" "high"))
(define %model-max-map
  (list (cons "id" "m")
    (cons "reasoning" #t)
    (cons "thinkingLevelMap" (list (cons "max" "max"))))
)
(check (pi-model-thinking-levels %model-max-map)
  => '("off" "minimal" "low" "medium" "high" "max"))
;; 6. 符号键与字符串键等价
(check (pi-model-thinking-levels '((id . "m") (reasoning . #t)))
  => '("off" "minimal" "low" "medium" "high"))

;; pi-clamp-thinking-level
;; 把请求级别收敛到最近的支持级别（先向高、再向低）。
;; 1. 支持的级别原样返回
(check (pi-clamp-thinking-level "high" '("off" "minimal" "low" "medium" "high")) => "high")
;; 2. 请求级别不支持时先向更高档找
(check (pi-clamp-thinking-level "medium" '("off" "minimal" "low" "high")) => "high")
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

;; pi-resolve-thinking-level
;; 生效级别解析：CLI > per-model > 全局 > 默认（medium），最后按能力收敛。
;; 1. CLI 优先
(check (pi-resolve-thinking-level "low" "high" "minimal" %levels) => "low")
;; 2. 无 CLI 时 per-model 覆盖优先于全局
(check (pi-resolve-thinking-level #f "high" "minimal" %levels) => "high")
;; 3. 只有全局默认时取全局
(check (pi-resolve-thinking-level #f #f "minimal" %levels) => "minimal")
;; 4. 都没有时取默认级别 medium
(check (pi-resolve-thinking-level #f #f #f %levels) => "medium")
;; 5. 空串视为未设置
(check (pi-resolve-thinking-level "" "" "" %levels) => "medium")
;; 6. 最终按模型可用级别收敛（模型不支持思考 → off）
(check (pi-resolve-thinking-level #f #f #f '("off")) => "off")
;; 7. 请求级别超出可用集时收敛到最近的支持级别
(check (pi-resolve-thinking-level "max" #f #f '("off" "minimal" "low" "medium" "high"))
  => "high")

(check-report)
