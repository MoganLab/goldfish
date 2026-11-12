;; pi-footer-model-label
;; 组装页脚右下角的单个模型标签串。
;;
;; 语法
;; ----
;; (pi-footer-model-label model-id thinking-level supports-thinking?)
;;
;; 参数
;; ----
;; model-id : string
;; 模型标识。
;; thinking-level : (or string #f)
;; 实际生效的思考等级。
;; supports-thinking? : boolean
;; 模型是否支持思考；#f 时不显示思考后缀。
;;
;; 返回值
;; ----
;; string
;; 支持思考时返回 "model • level"（off 也显式显示），否则只返回 model-id。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi code modes interactive footer-model)
) ;import

(check-set-mode! 'report-failed)

;; 模型显示串组装（对齐上游 pi）：
;; 支持 reasoning 时 off 也显式显示 " • thinking off"；
;; 不支持 reasoning 时只显示 model-id
(check (pi-footer-model-label "gemini-3.8-flash" "medium" #t)
  =>
  "gemini-3.8-flash • medium"
) ;check
(check (pi-footer-model-label "gemini-3.8-flash" "off" #t)
  =>
  "gemini-3.8-flash • thinking off"
) ;check
(check (pi-footer-model-label "kimi-for-coding" #f #f) => "kimi-for-coding")
(check (pi-footer-model-label "glm-5.3" "" #f) => "glm-5.3")
(check (pi-footer-model-label "glm-5.3" " high " #t) => "glm-5.3 • high")

;; 来自原 pi-app-thinking-test.scm 的 footer 标签断言。
;; 与上面相同的 medium/off/kimi/high 断言合并去重，仅保留新增覆盖。
(check (pi-footer-model-label "glm-5.3" "" #t) => "glm-5.3 • thinking off")

(check-report)
